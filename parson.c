/*
 Parson ( http://kgabis.github.com/parson/ )
 Copyright (c) 2012 - 2017 Krzysztof Gabis

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
*/
#ifdef _MSC_VER
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif /* _CRT_SECURE_NO_WARNINGS */
#endif /* _MSC_VER */

#include "parson.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>

/* Apparently sscanf is not implemented in some "standard" libraries, so don't use it, if you
 * don't have to. */
#define sscanf THINK_TWICE_ABOUT_USING_SSCANF

/* JSON 对象和 JSON 数组在创建之后的默认大小（可存储成员个数）*/
#define STARTING_CAPACITY 16

/* 表示 JSON 数据结构中支持的最大嵌套层数（或者说是在树形表示法中树的深度）*/
#define MAX_NESTING       2048

#define FLOAT_FORMAT "%1.17g" /* do not increase precision without incresing NUM_BUF_SIZE */
#define NUM_BUF_SIZE 64 /* double printed with "%1.17g" shouldn't be longer than 25 bytes so let's be paranoid and use 64 */

#define SIZEOF_TOKEN(a)       (sizeof(a) - 1)
#define SKIP_CHAR(str)        ((*str)++)
#define SKIP_WHITESPACES(str) while (isspace((unsigned char)(**str))) { SKIP_CHAR(str); }
#define MAX(a, b)             ((a) > (b) ? (a) : (b))

#undef malloc
#undef free

#if defined(isnan) && defined(isinf)
#define IS_NUMBER_INVALID(x) (isnan((x)) || isinf((x)))
#else
#define IS_NUMBER_INVALID(x) (((x) * 0.0) != 0.0)
#endif

static JSON_Malloc_Function parson_malloc = malloc;
static JSON_Free_Function parson_free = free;

static int parson_escape_slashes = 1;

#define IS_CONT(b) (((unsigned char)(b) & 0xC0) == 0x80) /* is utf-8 continuation byte */

/* Type definitions */

/*
 * 下面这定义了用     “树形结构”表示 JSON 数据需要的几个基础数据类型
 * 通过这些数据类型，我们可以把指定的 JSON 数据组织成树形结构并
 * 动态添加、移除或者修改 JSON 数据成员，需要注意的是这个树形结
 * 构表示了 JSON 数据成员之间的结构化关系，所以我们在这个基础上
 * 需要执行一个“序列化”操作后，才会形成字符串格式的 JSON 数据
 * 在我们对 JSON 数据结构动态操作时，需要动态内存管理的基础支持
 * 以实现动态创建和释放 JSON 数据成员结构
 */

/* 定义了 JSON 数据中会用到的几种“变量类型” */
typedef union json_value_value {
    char        *string;
    double       number;
    JSON_Object *object;
    JSON_Array  *array;
    int          boolean;
    int          null;
} JSON_Value_Value;

/* 定义一个 JSON 数据中的“变量”表示形式 */
struct json_value_t {
    JSON_Value      *parent;     /* 当前 JSON_Value 在“树形结构”表示中父节点指针 */
    JSON_Value_Type  type;       /* 当前 JSON_Value 变量类型 */
    JSON_Value_Value value;      /* 当前 JSON_Value 变量值 */
};

/*
 * 定义一个 JSON 数据中的“对象”表示形式
 * JSON 对象是按照“键值对”形式存储数据的，不同数据之间用","分隔
 */
struct json_object_t {
    JSON_Value  *wrapping_value; /* 当前 JSON object 所属 JSON_Value 的指针 */
    char       **names;          /* “键值对”中的“键”标识符 */
    JSON_Value **values;         /* “键值对”中的“值”标识符 */
    size_t       count;          /* 当前 JSON object 中已经存储的“键值对”个数 */
    size_t       capacity;       /* 当前 JSON object 最多可以存储的“键值对”个数 */
};

/* 定义一个 JSON 数据中的“数组”表示形式，数组中每个表示单位是 JSON_Value */
struct json_array_t {
    JSON_Value  *wrapping_value; /* 当前 JSON array 所属 JSON_Value 的指针 */
    JSON_Value **items;          /* 当前 JSON array 中所包含的 JSON_Value 数组首地址 */
    size_t       count;          /* 当前 JSON array 中已经存储的 JSON_Value 成员个数 */
    size_t       capacity;       /* 当前 JSON array 最多可以经存储的 JSON_Value 成员个数 */
};

/* Various */
static char * read_file(const char *filename);
static void   remove_comments(char *string, const char *start_token, const char *end_token);
static char * parson_strndup(const char *string, size_t n);
static char * parson_strdup(const char *string);
static int    hex_char_to_int(char c);
static int    parse_utf16_hex(const char *string, unsigned int *result);
static int    num_bytes_in_utf8_sequence(unsigned char c);
static int    verify_utf8_sequence(const unsigned char *string, int *len);
static int    is_valid_utf8(const char *string, size_t string_len);
static int    is_decimal(const char *string, size_t length);

/* JSON Object */
static JSON_Object * json_object_init(JSON_Value *wrapping_value);
static JSON_Status   json_object_add(JSON_Object *object, const char *name, JSON_Value *value);
static JSON_Status   json_object_addn(JSON_Object *object, const char *name, size_t name_len, JSON_Value *value);
static JSON_Status   json_object_resize(JSON_Object *object, size_t new_capacity);
static JSON_Value  * json_object_getn_value(const JSON_Object *object, const char *name, size_t name_len);
static JSON_Status   json_object_remove_internal(JSON_Object *object, const char *name, int free_value);
static JSON_Status   json_object_dotremove_internal(JSON_Object *object, const char *name, int free_value);
static void          json_object_free(JSON_Object *object);

/* JSON Array */
static JSON_Array * json_array_init(JSON_Value *wrapping_value);
static JSON_Status  json_array_add(JSON_Array *array, JSON_Value *value);
static JSON_Status  json_array_resize(JSON_Array *array, size_t new_capacity);
static void         json_array_free(JSON_Array *array);

/* JSON Value */
static JSON_Value * json_value_init_string_no_copy(char *string);

/* Parser */
static JSON_Status  skip_quotes(const char **string);
static int          parse_utf16(const char **unprocessed, char **processed);
static char *       process_string(const char *input, size_t len);
static char *       get_quoted_string(const char **string);
static JSON_Value * parse_object_value(const char **string, size_t nesting);
static JSON_Value * parse_array_value(const char **string, size_t nesting);
static JSON_Value * parse_string_value(const char **string);
static JSON_Value * parse_boolean_value(const char **string);
static JSON_Value * parse_number_value(const char **string);
static JSON_Value * parse_null_value(const char **string);
static JSON_Value * parse_value(const char **string, size_t nesting);

/* Serialization */
static int    json_serialize_to_buffer_r(const JSON_Value *value, char *buf, int level, int is_pretty, char *num_buf);
static int    json_serialize_string(const char *string, char *buf);
static int    append_indent(char *buf, int level);
static int    append_string(char *buf, const char *string);

/* Various */
/*********************************************************************************************************
** 函数名称: parson_strndup
** 功能描述: 分配内存并复制指定字符串的指定个数字符数据，然后返回字符串首地址
** 输     入: string - 要复制的字符串指针
**         : n - 要复制的字符创长度
** 输     出: output_string - 复制后的字符串指针
**         : NULL - 复制字符串失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static char * parson_strndup(const char *string, size_t n) {
    char *output_string = (char*)parson_malloc(n + 1);
    if (!output_string) {
        return NULL;
    }
    output_string[n] = '\0';
    strncpy(output_string, string, n);
    return output_string;
}

/*********************************************************************************************************
** 函数名称: parson_strdup
** 功能描述: 分配内存并复制指定字符串数据，然后返回字符串首地址
** 输     入: string - 要复制的字符串指针
** 输     出: output_string - 复制后的字符串指针
**         : NULL - 复制字符串失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static char * parson_strdup(const char *string) {
    return parson_strndup(string, strlen(string));
}

/*********************************************************************************************************
** 函数名称: hex_char_to_int
** 功能描述: 把指定的 hex 格式字符转换成与其对应的 ASCII 码值
** 输     入: c - hex 格式字符
** 输     出: int - 转换后的 ASCII 码值
**         : -1 - 输入字符非法
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static int hex_char_to_int(char c) {
    if (c >= '0' && c <= '9') {
        return c - '0';
    } else if (c >= 'a' && c <= 'f') {
        return c - 'a' + 10;
    } else if (c >= 'A' && c <= 'F') {
        return c - 'A' + 10;
    }
    return -1;
}

/*********************************************************************************************************
** 函数名称: parse_utf16_hex
** 功能描述: 把以 hex 字符串格式表示的 utf16 数据转换成与其对应的 int 类型值
** 输     入: s - hex 字符串格式的 utf16 数据
** 输     出: result - 转换成 int 类型的结果
**         : 1 - 执行成功
**         : 0 - 执行失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static int parse_utf16_hex(const char *s, unsigned int *result) {
    int x1, x2, x3, x4;
    if (s[0] == '\0' || s[1] == '\0' || s[2] == '\0' || s[3] == '\0') {
        return 0;
    }
    x1 = hex_char_to_int(s[0]);
    x2 = hex_char_to_int(s[1]);
    x3 = hex_char_to_int(s[2]);
    x4 = hex_char_to_int(s[3]);
    if (x1 == -1 || x2 == -1 || x3 == -1 || x4 == -1) {
        return 0;
    }
    *result = (unsigned int)((x1 << 12) | (x2 << 8) | (x3 << 4) | x4);
    return 1;
}

static int num_bytes_in_utf8_sequence(unsigned char c) {
    if (c == 0xC0 || c == 0xC1 || c > 0xF4 || IS_CONT(c)) {
        return 0;
    } else if ((c & 0x80) == 0) {    /* 0xxxxxxx */
        return 1;
    } else if ((c & 0xE0) == 0xC0) { /* 110xxxxx */
        return 2;
    } else if ((c & 0xF0) == 0xE0) { /* 1110xxxx */
        return 3;
    } else if ((c & 0xF8) == 0xF0) { /* 11110xxx */
        return 4;
    }
    return 0; /* won't happen */
}

static int verify_utf8_sequence(const unsigned char *string, int *len) {
    unsigned int cp = 0;
    *len = num_bytes_in_utf8_sequence(string[0]);

    if (*len == 1) {
        cp = string[0];
    } else if (*len == 2 && IS_CONT(string[1])) {
        cp = string[0] & 0x1F;
        cp = (cp << 6) | (string[1] & 0x3F);
    } else if (*len == 3 && IS_CONT(string[1]) && IS_CONT(string[2])) {
        cp = ((unsigned char)string[0]) & 0xF;
        cp = (cp << 6) | (string[1] & 0x3F);
        cp = (cp << 6) | (string[2] & 0x3F);
    } else if (*len == 4 && IS_CONT(string[1]) && IS_CONT(string[2]) && IS_CONT(string[3])) {
        cp = string[0] & 0x7;
        cp = (cp << 6) | (string[1] & 0x3F);
        cp = (cp << 6) | (string[2] & 0x3F);
        cp = (cp << 6) | (string[3] & 0x3F);
    } else {
        return 0;
    }

    /* overlong encodings */
    if ((cp < 0x80    && *len > 1) ||
        (cp < 0x800   && *len > 2) ||
        (cp < 0x10000 && *len > 3)) {
        return 0;
    }

    /* invalid unicode */
    if (cp > 0x10FFFF) {
        return 0;
    }

    /* surrogate halves */
    if (cp >= 0xD800 && cp <= 0xDFFF) {
        return 0;
    }

    return 1;
}

static int is_valid_utf8(const char *string, size_t string_len) {
    int len = 0;
    const char *string_end =  string + string_len;
    while (string < string_end) {
        if (!verify_utf8_sequence((const unsigned char*)string, &len)) {
            return 0;
        }
        string += len;
    }
    return 1;
}

static int is_decimal(const char *string, size_t length) {
    if (length > 1 && string[0] == '0' && string[1] != '.') {
        return 0;
    }
    if (length > 2 && !strncmp(string, "-0", 2) && string[2] != '.') {
        return 0;
    }
    while (length--) {
        if (strchr("xX", string[length])) {
            return 0;
        }
    }
    return 1;
}

/*********************************************************************************************************
** 函数名称: read_file
** 功能描述: 读取指定 JSON 文件名的数据到动态分配的缓冲区中，并返回这个缓冲区首地址
** 输     入: filename - 要读取的 JSON 文件名
** 输     出: file_contents - 读取到 JSON 数据的缓冲区首地址
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static char * read_file(const char * filename) {
    FILE *fp = fopen(filename, "r");
    size_t size_to_read = 0;
    size_t size_read = 0;
    long pos;
    char *file_contents;
    if (!fp) {
        return NULL;
    }
    fseek(fp, 0L, SEEK_END);
    pos = ftell(fp);
    if (pos < 0) {
        fclose(fp);
        return NULL;
    }
    size_to_read = pos;
    rewind(fp);
    file_contents = (char*)parson_malloc(sizeof(char) * (size_to_read + 1));
    if (!file_contents) {
        fclose(fp);
        return NULL;
    }
    size_read = fread(file_contents, 1, size_to_read, fp);
    if (size_read == 0 || ferror(fp)) {
        fclose(fp);
        parson_free(file_contents);
        return NULL;
    }
    fclose(fp);
    file_contents[size_read] = '\0';
    return file_contents;
}

/*********************************************************************************************************
** 函数名称: remove_comments
** 功能描述: 从指定的字符串中移除和 JSON 数据无关的注释信息，注释信息是指在起始标识符和结束标识符之间的内容
** 输     入: start_token - 注释信息的起始标识符，例如 /* 和 //
*///       : end_token - 注释信息的结束标识符 */ 和 \n                                                
/* 输     出: string - 移除了注释信息的 JSON 数据
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static void remove_comments(char *string, const char *start_token, const char *end_token) {
    int in_string = 0, escaped = 0;
    size_t i;
    char *ptr = NULL, current_char;
    size_t start_token_len = strlen(start_token);
    size_t end_token_len = strlen(end_token);
    if (start_token_len == 0 || end_token_len == 0) {
        return;
    }
    while ((current_char = *string) != '\0') {
        if (current_char == '\\' && !escaped) {
            escaped = 1;
            string++;
            continue;
        } else if (current_char == '\"' && !escaped) {
            in_string = !in_string;
        } else if (!in_string && strncmp(string, start_token, start_token_len) == 0) {
            for(i = 0; i < start_token_len; i++) {
                string[i] = ' ';
            }
            string = string + start_token_len;
            ptr = strstr(string, end_token);
            if (!ptr) {
                return;
            }
            for (i = 0; i < (ptr - string) + end_token_len; i++) {
                string[i] = ' ';
            }
            string = ptr + end_token_len - 1;
        }
        escaped = 0;
        string++;
    }
}

/* JSON Object */
/*********************************************************************************************************
** 函数名称: json_object_init
** 功能描述: 创建并初始化一个 JSON Object 变量
** 输     入: wrapping_value - 新创建的 JSON Object 所属 JSON_Value 指针
** 输     出: new_obj - 新的 JSON Object 变量
**         : NULL - 执行失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Object * json_object_init(JSON_Value *wrapping_value) {
    JSON_Object *new_obj = (JSON_Object*)parson_malloc(sizeof(JSON_Object));
    if (new_obj == NULL) {
        return NULL;
    }
    new_obj->wrapping_value = wrapping_value;
    new_obj->names = (char**)NULL;
    new_obj->values = (JSON_Value**)NULL;
    new_obj->capacity = 0;
    new_obj->count = 0;
    return new_obj;
}

/*********************************************************************************************************
** 函数名称: json_object_add
** 功能描述: 向指定的 JSON object 中添加一个新的“键值对”成员
** 输     入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
**         : value - “键值对”的“值”标识符内容
** 输     出: JSON_Status - 执行状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Status json_object_add(JSON_Object *object, const char *name, JSON_Value *value) {
    if (name == NULL) {
        return JSONFailure;
    }
    return json_object_addn(object, name, strlen(name), value);
}

/*********************************************************************************************************
** 函数名称: json_object_addn
** 功能描述: 向指定的 JSON object 中添加一个新的“键值对”成员，需要我们指定“键”标识符的长度
** 输     入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
**         : name_len - “键值对”的“键”标识符长度
**         : value - “键值对”的“值”标识符内容
** 输     出: JSON_Status - 执行状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Status json_object_addn(JSON_Object *object, const char *name, size_t name_len, JSON_Value *value) {
    size_t index = 0;
    if (object == NULL || name == NULL || value == NULL) {
        return JSONFailure;
    }
    if (json_object_getn_value(object, name, name_len) != NULL) {
        return JSONFailure;
    }
    if (object->count >= object->capacity) {
        size_t new_capacity = MAX(object->capacity * 2, STARTING_CAPACITY);
        if (json_object_resize(object, new_capacity) == JSONFailure) {
            return JSONFailure;
        }
    }
    index = object->count;
    object->names[index] = parson_strndup(name, name_len);
    if (object->names[index] == NULL) {
        return JSONFailure;
    }
    value->parent = json_object_get_wrapping_value(object);
    object->values[index] = value;
    object->count++;
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_object_resize
** 功能描述: 把指定的 JSON object 的 capacity 设置成指定的新值
** 注     释:  一个 JSON object 可以按照“键值对”的方式存储数据，在指定的 JSON object 中 capacity 字段表示的
**         : 是这个 JSON object 可以存储多少个“键值对”
** 输     入: object - 我们要操作的 JSON object 对象
**         : new_capacity - 新的存储空间大小
** 输     出: JSON_Status - 执行状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Status json_object_resize(JSON_Object *object, size_t new_capacity) {
    char **temp_names = NULL;
    JSON_Value **temp_values = NULL;

    if ((object->names == NULL && object->values != NULL) ||
        (object->names != NULL && object->values == NULL) ||
        new_capacity == 0) {
            return JSONFailure; /* Shouldn't happen */
    }
    temp_names = (char**)parson_malloc(new_capacity * sizeof(char*));
    if (temp_names == NULL) {
        return JSONFailure;
    }
    temp_values = (JSON_Value**)parson_malloc(new_capacity * sizeof(JSON_Value*));
    if (temp_values == NULL) {
        parson_free(temp_names);
        return JSONFailure;
    }
    if (object->names != NULL && object->values != NULL && object->count > 0) {
        memcpy(temp_names, object->names, object->count * sizeof(char*));
        memcpy(temp_values, object->values, object->count * sizeof(JSON_Value*));
    }
    parson_free(object->names);
    parson_free(object->values);
    object->names = temp_names;
    object->values = temp_values;
    object->capacity = new_capacity;
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_object_getn_value
** 功能描述: 在指定的 JSON object 中，通过“键值对”中的“键”标识符获取与其对应的“值”标识符的内容
** 输     入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
**         : name_len - “键值对”的“键”标识符长度
** 输     出: JSON_Status - 执行状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Value * json_object_getn_value(const JSON_Object *object, const char *name, size_t name_len) {
    size_t i, name_length;
    for (i = 0; i < json_object_get_count(object); i++) {
        name_length = strlen(object->names[i]);
        if (name_length != name_len) {
            continue;
        }
        if (strncmp(object->names[i], name, name_len) == 0) {
            return object->values[i];
        }
    }
    return NULL;
}

/*********************************************************************************************************
** 函数名称: json_object_remove_internal
** 功能描述: 从指定的 JSON object 中通过“键值对”的“键”标识符找到与其对应的成员并删除
** 输     入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
**         : free_value - 是否释放“键值对”的“值”标识符占用的资源
** 输     出: JSON_Status - 执行状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Status json_object_remove_internal(JSON_Object *object, const char *name, int free_value) {
    size_t i = 0, last_item_index = 0;
    if (object == NULL || json_object_get_value(object, name) == NULL) {
        return JSONFailure;
    }
    last_item_index = json_object_get_count(object) - 1;
    for (i = 0; i < json_object_get_count(object); i++) {
        if (strcmp(object->names[i], name) == 0) {
            parson_free(object->names[i]);
            if (free_value) {
                json_value_free(object->values[i]);
            }
            if (i != last_item_index) { /* Replace key value pair with one from the end */
                object->names[i] = object->names[last_item_index];
                object->values[i] = object->values[last_item_index];
            }
            object->count -= 1;
            return JSONSuccess;
        }
    }
    return JSONFailure; /* No execution path should end here */
}

/*********************************************************************************************************
** 函数名称: json_object_dotremove_internal
** 功能描述: 从指定的 JSON object 中通过“点表示法”找到与其对应的成员并删除
** 输     入: object - 我们要操作的 JSON object 对象
**         : name - “点表示法”表示的“键值对”的“键”标识符
**         : free_value - 是否释放“键值对”的“值”标识符占用的资源
** 输     出: JSON_Status - 执行状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Status json_object_dotremove_internal(JSON_Object *object, const char *name, int free_value) {
    JSON_Value *temp_value = NULL;
    JSON_Object *temp_object = NULL;
    const char *dot_pos = strchr(name, '.');
    if (dot_pos == NULL) {
        return json_object_remove_internal(object, name, free_value);
    }
    temp_value = json_object_getn_value(object, name, dot_pos - name);
    if (json_value_get_type(temp_value) != JSONObject) {
        return JSONFailure;
    }
    temp_object = json_value_get_object(temp_value);
    return json_object_dotremove_internal(temp_object, dot_pos + 1, free_value);
}

/*********************************************************************************************************
** 函数名称: json_object_free
** 功能描述: 释放一个 JSON object 类型成员所占用的所有内存空间
** 输     入: object - 我们要操作的 JSON object 对象
** 输     出: 
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static void json_object_free(JSON_Object *object) {
    size_t i;
    for (i = 0; i < object->count; i++) {
        parson_free(object->names[i]);
        json_value_free(object->values[i]);
    }
    parson_free(object->names);
    parson_free(object->values);
    parson_free(object);
}

/* JSON Array */
/*********************************************************************************************************
** 函数名称: json_array_init
** 功能描述: 创建并初始化一个 JSON Array 变量
** 输     入: wrapping_value - 新创建的 JSON Array 所属 JSON_Value 指针
** 输     出: new_array - 新创建并初始化的 JSON_Array 指针
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Array * json_array_init(JSON_Value *wrapping_value) {
    JSON_Array *new_array = (JSON_Array*)parson_malloc(sizeof(JSON_Array));
    if (new_array == NULL) {
        return NULL;
    }
    new_array->wrapping_value = wrapping_value;
    new_array->items = (JSON_Value**)NULL;
    new_array->capacity = 0;
    new_array->count = 0;
    return new_array;
}

/*********************************************************************************************************
** 函数名称: json_array_add
** 功能描述: 向指定的 JSON array 中添加一个新的 JSON_Value 成员
** 输     入: array - 我们要操作的 JSON array 对象
**         : value - 新添加的数组成员单位
** 输     出: JSON_Status - 执行状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Status json_array_add(JSON_Array *array, JSON_Value *value) {
    if (array->count >= array->capacity) {
        size_t new_capacity = MAX(array->capacity * 2, STARTING_CAPACITY);
        if (json_array_resize(array, new_capacity) == JSONFailure) {
            return JSONFailure;
        }
    }
    value->parent = json_array_get_wrapping_value(array);
    array->items[array->count] = value;
    array->count++;
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_resize
** 功能描述: 把指定的 JSON array 的 capacity 设置成指定的新值
** 注     释: 一个 JSON array 可以按照数组的方式存储 JSON_Value 数据，在指定的 JSON array 中 capacity 字段
**         : 表示的是这个 JSON array 可以存储多少个 JSON_Value
** 输     入: array - 我们要操作的 JSON array 对象
**         : new_capacity - 新的存储空间大小
** 输     出: JSON_Status - 执行状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Status json_array_resize(JSON_Array *array, size_t new_capacity) {
    JSON_Value **new_items = NULL;
    if (new_capacity == 0) {
        return JSONFailure;
    }
    new_items = (JSON_Value**)parson_malloc(new_capacity * sizeof(JSON_Value*));
    if (new_items == NULL) {
        return JSONFailure;
    }
    if (array->items != NULL && array->count > 0) {
        memcpy(new_items, array->items, array->count * sizeof(JSON_Value*));
    }
    parson_free(array->items);
    array->items = new_items;
    array->capacity = new_capacity;
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_free
** 功能描述: 释放一个 JSON array 类型成员所占用的所有内存空间
** 输     入: array - 我们要操作的 JSON array 对象
** 输     出: 
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static void json_array_free(JSON_Array *array) {
    size_t i;
    for (i = 0; i < array->count; i++) {
        json_value_free(array->items[i]);
    }
    parson_free(array->items);
    parson_free(array);
}

/* JSON Value */
/*********************************************************************************************************
** 函数名称: json_value_init_string_no_copy
** 功能描述: 创建并初始化一个             JSONString 类型的 JSON_Value 变量
** 输     入: string - 新创建的变量需要初始化成的内容
** 输     出: new_value - 新创建并初始化的 JSONString 类型的 JSON_Array 指针
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Value * json_value_init_string_no_copy(char *string) {
    JSON_Value *new_value = (JSON_Value*)parson_malloc(sizeof(JSON_Value));
    if (!new_value) {
        return NULL;
    }
    new_value->parent = NULL;
    new_value->type = JSONString;
    new_value->value.string = string;
    return new_value;
}

/* Parser */
/*********************************************************************************************************
** 函数名称: skip_quotes
** 功能描述: 找到第一个双引号对的位置，比如函数参数的内容为：
**         : "name" : "zhaogezhang"，在调用完这个函数之后，*string 指向的位置如下：
**         :       ^
**         : 即第一个双引号对后的第一个字符位置
** 输     入: string - 需要处理的字符串指针
** 输     出: JSON_Status - 执行状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Status skip_quotes(const char **string) {
    if (**string != '\"') {
        return JSONFailure;
    }
    SKIP_CHAR(string);
    while (**string != '\"') {
        if (**string == '\0') {
            return JSONFailure;
        } else if (**string == '\\') {
            SKIP_CHAR(string);
            if (**string == '\0') {
                return JSONFailure;
            }
        }
        SKIP_CHAR(string);
    }
    SKIP_CHAR(string);
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: parse_utf16
** 功能描述: 把 utf16 编码格式数据转换成与其对应的字符串格式数据
** 输     入: unprocessed - 需要被转换的 utf16 格式编码数据
**         : processed - 存储转换结果的缓冲区
** 输     出: JSON_Status - 执行状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static int parse_utf16(const char **unprocessed, char **processed) {
    unsigned int cp, lead, trail;
    int parse_succeeded = 0;
    char *processed_ptr = *processed;
    const char *unprocessed_ptr = *unprocessed;
    unprocessed_ptr++; /* skips u */
    parse_succeeded = parse_utf16_hex(unprocessed_ptr, &cp);
    if (!parse_succeeded) {
        return JSONFailure;
    }
    if (cp < 0x80) {
        processed_ptr[0] = (char)cp; /* 0xxxxxxx */
    } else if (cp < 0x800) {
        processed_ptr[0] = ((cp >> 6) & 0x1F) | 0xC0; /* 110xxxxx */
        processed_ptr[1] = ((cp)      & 0x3F) | 0x80; /* 10xxxxxx */
        processed_ptr += 1;
    } else if (cp < 0xD800 || cp > 0xDFFF) {
        processed_ptr[0] = ((cp >> 12) & 0x0F) | 0xE0; /* 1110xxxx */
        processed_ptr[1] = ((cp >> 6)  & 0x3F) | 0x80; /* 10xxxxxx */
        processed_ptr[2] = ((cp)       & 0x3F) | 0x80; /* 10xxxxxx */
        processed_ptr += 2;
    } else if (cp >= 0xD800 && cp <= 0xDBFF) { /* lead surrogate (0xD800..0xDBFF) */
        lead = cp;
        unprocessed_ptr += 4; /* should always be within the buffer, otherwise previous sscanf would fail */
        if (*unprocessed_ptr++ != '\\' || *unprocessed_ptr++ != 'u') {
            return JSONFailure;
        }
        parse_succeeded = parse_utf16_hex(unprocessed_ptr, &trail);
        if (!parse_succeeded || trail < 0xDC00 || trail > 0xDFFF) { /* valid trail surrogate? (0xDC00..0xDFFF) */
            return JSONFailure;
        }
        cp = ((((lead - 0xD800) & 0x3FF) << 10) | ((trail - 0xDC00) & 0x3FF)) + 0x010000;
        processed_ptr[0] = (((cp >> 18) & 0x07) | 0xF0); /* 11110xxx */
        processed_ptr[1] = (((cp >> 12) & 0x3F) | 0x80); /* 10xxxxxx */
        processed_ptr[2] = (((cp >> 6)  & 0x3F) | 0x80); /* 10xxxxxx */
        processed_ptr[3] = (((cp)       & 0x3F) | 0x80); /* 10xxxxxx */
        processed_ptr += 3;
    } else { /* trail surrogate before lead surrogate */
        return JSONFailure;
    }
    unprocessed_ptr += 3;
    *processed = processed_ptr;
    *unprocessed = unprocessed_ptr;
    return JSONSuccess;
}


/* Copies and processes passed string up to supplied length.
Example: "\u006Corem ipsum" -> lorem ipsum */
/*********************************************************************************************************
** 函数名称: process_string
** 功能描述: 把不同格式的输入字符串数据转换成与其对应的 ascii 字符串格式
** 输     入: input - 需要转换的不同格式数据
**         : len - 我们需要转换的字符长度
** 输     出: output - 转换后 的 ascii 字符串格式地址
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static char* process_string(const char *input, size_t len) {
    const char *input_ptr = input;
    size_t initial_size = (len + 1) * sizeof(char);
    size_t final_size = 0;
    char *output = NULL, *output_ptr = NULL, *resized_output = NULL;
    output = (char*)parson_malloc(initial_size);
    if (output == NULL) {
        goto error;
    }
    output_ptr = output;
    while ((*input_ptr != '\0') && (size_t)(input_ptr - input) < len) {
        if (*input_ptr == '\\') {
            input_ptr++;
            switch (*input_ptr) {
                case '\"': *output_ptr = '\"'; break;
                case '\\': *output_ptr = '\\'; break;
                case '/':  *output_ptr = '/';  break;
                case 'b':  *output_ptr = '\b'; break;
                case 'f':  *output_ptr = '\f'; break;
                case 'n':  *output_ptr = '\n'; break;
                case 'r':  *output_ptr = '\r'; break;
                case 't':  *output_ptr = '\t'; break;
                case 'u':
                    if (parse_utf16(&input_ptr, &output_ptr) == JSONFailure) {
                        goto error;
                    }
                    break;
                default:
                    goto error;
            }
        } else if ((unsigned char)*input_ptr < 0x20) {
            goto error; /* 0x00-0x19 are invalid characters for JSON string (http://www.ietf.org/rfc/rfc4627.txt) */
        } else {
            *output_ptr = *input_ptr;
        }
        output_ptr++;
        input_ptr++;
    }
    *output_ptr = '\0';
    /* resize to new length */
    final_size = (size_t)(output_ptr-output) + 1;
    /* todo: don't resize if final_size == initial_size */
    resized_output = (char*)parson_malloc(final_size);
    if (resized_output == NULL) {
        goto error;
    }
    memcpy(resized_output, output, final_size);
    parson_free(output);
    return resized_output;
error:
    parson_free(output);
    return NULL;
}

/* Return processed contents of a string between quotes and
   skips passed argument to a matching quote. */
/*********************************************************************************************************
** 函数名称: get_quoted_string
** 功能描述: 提取以“双引号”开头的字符串中，第一个“双引号对”中的数据内容，并将这些数据转换成与其对应的
**         : ascii 字符串格式并返回，例如：
**         : if arg string = "name": "zhaoge.zhang"
**         : return char * =  name
** 输     入: string - 需要提取的字符串指针
** 输     出: output - 转换后的 ascii 字符串格式地址
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static char * get_quoted_string(const char **string) {
    const char *string_start = *string;
    size_t string_len = 0;
    JSON_Status status = skip_quotes(string);
    if (status != JSONSuccess) {
        return NULL;
    }
    string_len = *string - string_start - 2; /* length without quotes */
    return process_string(string_start + 1, string_len);
}

/*********************************************************************************************************
** 函数名称: parse_value
** 功能描述: 解析指定的 JSON 字符串数据，将其转换成“树形结构”表示形式
** 输     入: string - 需要解析的 JSON 字符串
**         : nesting - 当前解析的 JSON 字符串在整个 JSON 数据中的嵌套层数
** 输     出: JSON_Value - 转换后的“树形结构” JSON 数据
**         : NULL - 转换失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Value * parse_value(const char **string, size_t nesting) {
    if (nesting > MAX_NESTING) {
        return NULL;
    }
    SKIP_WHITESPACES(string);
    switch (**string) {
        case '{':
            return parse_object_value(string, nesting + 1);
        case '[':
            return parse_array_value(string, nesting + 1);
        case '\"':
            return parse_string_value(string);
        case 'f': case 't':
            return parse_boolean_value(string);
        case '-':
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            return parse_number_value(string);
        case 'n':
            return parse_null_value(string);
        default:
            return NULL;
    }
}

/*********************************************************************************************************
** 函数名称: parse_object_value
** 功能描述: 把“序列化”格式的字符串 JSON object 解析并转换成与其对应的“树形结构”格式的 JSON_Value 数据
** 输     入: string - 需要解析的“序列化”格式的字符串
**         : nesting - 当前 JSON object 在整个 JSON 数据中的嵌套层数
** 输     出: JSON_Value - 转换后的“树形结构”格式的 JSON_Value 数据
**         : NULL - 转换失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Value * parse_object_value(const char **string, size_t nesting) {
    JSON_Value *output_value = NULL, *new_value = NULL;
    JSON_Object *output_object = NULL;
    char *new_key = NULL;
    output_value = json_value_init_object();
    if (output_value == NULL) {
        return NULL;
    }
    if (**string != '{') {
        json_value_free(output_value);
        return NULL;
    }
    output_object = json_value_get_object(output_value);
    SKIP_CHAR(string);
    SKIP_WHITESPACES(string);
    if (**string == '}') { /* empty object */
        SKIP_CHAR(string);
        return output_value;
    }
    while (**string != '\0') {
        new_key = get_quoted_string(string);
        if (new_key == NULL) {
            json_value_free(output_value);
            return NULL;
        }
        SKIP_WHITESPACES(string);
        if (**string != ':') {
            parson_free(new_key);
            json_value_free(output_value);
            return NULL;
        }
        SKIP_CHAR(string);
        new_value = parse_value(string, nesting);
        if (new_value == NULL) {
            parson_free(new_key);
            json_value_free(output_value);
            return NULL;
        }
        if (json_object_add(output_object, new_key, new_value) == JSONFailure) {
            parson_free(new_key);
            json_value_free(new_value);
            json_value_free(output_value);
            return NULL;
        }
        parson_free(new_key);
        SKIP_WHITESPACES(string);
        if (**string != ',') {
            break;
        }
        SKIP_CHAR(string);
        SKIP_WHITESPACES(string);
    }
    SKIP_WHITESPACES(string);
    if (**string != '}' || /* Trim object after parsing is over */
        json_object_resize(output_object, json_object_get_count(output_object)) == JSONFailure) {
            json_value_free(output_value);
            return NULL;
    }
    SKIP_CHAR(string);
    return output_value;
}

/*********************************************************************************************************
** 函数名称: parse_array_value
** 功能描述: 把“序列化”格式的字符串 JSON array 解析并转换成与其对应的“树形结构”格式的 JSON_Value 数据
** 输     入: string - 需要解析的“序列化”格式的字符串
**         : nesting - 当前 JSON object 在整个 JSON 数据中的嵌套层数
** 输     出: JSON_Value - 转换后的“树形结构”格式的 JSON_Value 数据
**         : NULL - 转换失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Value * parse_array_value(const char **string, size_t nesting) {
    JSON_Value *output_value = NULL, *new_array_value = NULL;
    JSON_Array *output_array = NULL;
    output_value = json_value_init_array();
    if (output_value == NULL) {
        return NULL;
    }
    if (**string != '[') {
        json_value_free(output_value);
        return NULL;
    }
    output_array = json_value_get_array(output_value);
    SKIP_CHAR(string);
    SKIP_WHITESPACES(string);
    if (**string == ']') { /* empty array */
        SKIP_CHAR(string);
        return output_value;
    }
    while (**string != '\0') {
        new_array_value = parse_value(string, nesting);
        if (new_array_value == NULL) {
            json_value_free(output_value);
            return NULL;
        }
        if (json_array_add(output_array, new_array_value) == JSONFailure) {
            json_value_free(new_array_value);
            json_value_free(output_value);
            return NULL;
        }
        SKIP_WHITESPACES(string);
        if (**string != ',') {
            break;
        }
        SKIP_CHAR(string);
        SKIP_WHITESPACES(string);
    }
    SKIP_WHITESPACES(string);
    if (**string != ']' || /* Trim array after parsing is over */
        json_array_resize(output_array, json_array_get_count(output_array)) == JSONFailure) {
            json_value_free(output_value);
            return NULL;
    }
    SKIP_CHAR(string);
    return output_value;
}

/*********************************************************************************************************
** 函数名称: parse_string_value
** 功能描述: 把“序列化”的双引号格式的 JSON string 解析并转换成与其对应的“树形结构”格式的 JSON_Value 数据
** 输     入: string - 需要解析的“序列化”的双引号格式的字符串
** 输     出: JSON_Value - 转换后的“树形结构”格式的 JSON_String 数据
**         : NULL - 转换失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Value * parse_string_value(const char **string) {
    JSON_Value *value = NULL;
    char *new_string = get_quoted_string(string);
    if (new_string == NULL) {
        return NULL;
    }
    value = json_value_init_string_no_copy(new_string);
    if (value == NULL) {
        parson_free(new_string);
        return NULL;
    }
    return value;
}

/*********************************************************************************************************
** 函数名称: parse_boolean_value
** 功能描述: 把“序列化”的 JSON bool 类型变量解析并转换成与其对应的“树形结构”格式的 JSON_Value 数据
** 输     入: string - 需要解析的“序列化”的 JSON bool 字符串
** 输     出: JSON_Value - 转换后的“树形结构”格式的 JSON_Bool 数据
**         : NULL - 转换失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Value * parse_boolean_value(const char **string) {
    size_t true_token_size = SIZEOF_TOKEN("true");
    size_t false_token_size = SIZEOF_TOKEN("false");
    if (strncmp("true", *string, true_token_size) == 0) {
        *string += true_token_size;
        return json_value_init_boolean(1);
    } else if (strncmp("false", *string, false_token_size) == 0) {
        *string += false_token_size;
        return json_value_init_boolean(0);
    }
    return NULL;
}

/*********************************************************************************************************
** 函数名称: parse_number_value
** 功能描述: 把“序列化”的 JSON number 类型变量解析并转换成与其对应的“树形结构”格式的 JSON_Value 数据
** 输     入: string - 需要解析的“序列化”的 JSON number 字符串
** 输     出: JSON_Value - 转换后的“树形结构”格式的 JSON_Number 数据
**         : NULL - 转换失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Value * parse_number_value(const char **string) {
    char *end;
    double number = 0;
    errno = 0;
    number = strtod(*string, &end);
    if (errno || !is_decimal(*string, end - *string)) {
        return NULL;
    }
    *string = end;
    return json_value_init_number(number);
}

/*********************************************************************************************************
** 函数名称: parse_null_value
** 功能描述: 把“序列化”的 JSON null 类型变量解析并转换成与其对应的“树形结构”格式的 JSON_Value 数据
** 输     入: string - 需要解析的“序列化”的 JSON null 字符串
** 输     出: JSON_Value - 转换后的“树形结构”格式的 JSON_Null 数据
**         : NULL - 转换失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static JSON_Value * parse_null_value(const char **string) {
    size_t token_size = SIZEOF_TOKEN("null");
    if (strncmp("null", *string, token_size) == 0) {
        *string += token_size;
        return json_value_init_null();
    }
    return NULL;
}

/* Serialization */
#define APPEND_STRING(str) do { written = append_string(buf, (str));\
                                if (written < 0) { return -1; }\
                                if (buf != NULL) { buf += written; }\
                                written_total += written; } while(0)

#define APPEND_INDENT(level) do { written = append_indent(buf, (level));\
                                  if (written < 0) { return -1; }\
                                  if (buf != NULL) { buf += written; }\
                                  written_total += written; } while(0)

/*********************************************************************************************************
** 函数名称: json_serialize_to_buffer_r
** 功能描述: 把指定的 JSON 数据“树形结构”的表示形式数据转换成与其对应的“序列化”格式的字符串数据并把转换后
**         : 的结果存储到我们指定的缓存空间中
** 输	 入: value - 需要转换的“树形结构” JSON 数据
**         : buf - 用来存储转换后的、“序列化”格式的字符串缓冲区
**         : level - 在“序列化”的字符串中，不同 JSON 对象之间需要添加的空格数量，单位是 4 个空格
**         : is_pretty - 我们在“序列化”后的字符串中是否需要添加格式化空格来提高可阅读性
**         : num_buf - 用来存储 JSONNumber 类型变量值的缓冲区
** 输	 出: written_total - 我们序列化后的字符串长度，不包括字符串结尾'\0'字符
**		   : -1 - 执行失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static int json_serialize_to_buffer_r(const JSON_Value *value, char *buf, int level, int is_pretty, char *num_buf)
{
    const char *key = NULL, *string = NULL;
    JSON_Value *temp_value = NULL;
    JSON_Array *array = NULL;
    JSON_Object *object = NULL;
    size_t i = 0, count = 0;
    double num = 0.0;
    int written = -1, written_total = 0;

    switch (json_value_get_type(value)) {
        case JSONArray:
            array = json_value_get_array(value);
            count = json_array_get_count(array);
            APPEND_STRING("[");
            if (count > 0 && is_pretty) {
                APPEND_STRING("\n");
            }
            for (i = 0; i < count; i++) {
                if (is_pretty) {
                    APPEND_INDENT(level+1);
                }
                temp_value = json_array_get_value(array, i);
                written = json_serialize_to_buffer_r(temp_value, buf, level+1, is_pretty, num_buf);
                if (written < 0) {
                    return -1;
                }
                if (buf != NULL) {
                    buf += written;
                }
                written_total += written;
                if (i < (count - 1)) {
                    APPEND_STRING(",");
                }
                if (is_pretty) {
                    APPEND_STRING("\n");
                }
            }
            if (count > 0 && is_pretty) {
                APPEND_INDENT(level);
            }
            APPEND_STRING("]");
            return written_total;
        case JSONObject:
            object = json_value_get_object(value);
            count  = json_object_get_count(object);
            APPEND_STRING("{");
            if (count > 0 && is_pretty) {
                APPEND_STRING("\n");
            }
            for (i = 0; i < count; i++) {
                key = json_object_get_name(object, i);
                if (key == NULL) {
                    return -1;
                }
                if (is_pretty) {
                    APPEND_INDENT(level+1);
                }
                written = json_serialize_string(key, buf);
                if (written < 0) {
                    return -1;
                }
                if (buf != NULL) {
                    buf += written;
                }
                written_total += written;
                APPEND_STRING(":");
                if (is_pretty) {
                    APPEND_STRING(" ");
                }
                temp_value = json_object_get_value(object, key);
                written = json_serialize_to_buffer_r(temp_value, buf, level+1, is_pretty, num_buf);
                if (written < 0) {
                    return -1;
                }
                if (buf != NULL) {
                    buf += written;
                }
                written_total += written;
                if (i < (count - 1)) {
                    APPEND_STRING(",");
                }
                if (is_pretty) {
                    APPEND_STRING("\n");
                }
            }
            if (count > 0 && is_pretty) {
                APPEND_INDENT(level);
            }
            APPEND_STRING("}");
            return written_total;
        case JSONString:
            string = json_value_get_string(value);
            if (string == NULL) {
                return -1;
            }
            written = json_serialize_string(string, buf);
            if (written < 0) {
                return -1;
            }
            if (buf != NULL) {
                buf += written;
            }
            written_total += written;
            return written_total;
        case JSONBoolean:
            if (json_value_get_boolean(value)) {
                APPEND_STRING("true");
            } else {
                APPEND_STRING("false");
            }
            return written_total;
        case JSONNumber:
            num = json_value_get_number(value);
            if (buf != NULL) {
                num_buf = buf;
            }
            written = sprintf(num_buf, FLOAT_FORMAT, num);
            if (written < 0) {
                return -1;
            }
            if (buf != NULL) {
                buf += written;
            }
            written_total += written;
            return written_total;
        case JSONNull:
            APPEND_STRING("null");
            return written_total;
        case JSONError:
            return -1;
        default:
            return -1;
    }
}

/*********************************************************************************************************
** 函数名称: json_serialize_string
** 功能描述: 把指定的 JSON string 数据转换成与其对应的“序列化”格式的字符串数据并把转换后的结果存储到我们
**         : 指定的缓存空间中（常常用于序列化“键值对”中的“键”描述符字段）
** 输	 入: string - 我们需要序列化的 JSON string 字符串
**         : buf - 存储序列化后结果的缓冲区起始地址
** 输	 出: written_total - 一共向 buf 中写入数据的字节数
**		   : -1 - 执行失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static int json_serialize_string(const char *string, char *buf) {
    size_t i = 0, len = strlen(string);
    char c = '\0';
    int written = -1, written_total = 0;
    APPEND_STRING("\"");
    for (i = 0; i < len; i++) {
        c = string[i];
        switch (c) {
            case '\"': APPEND_STRING("\\\""); break;
            case '\\': APPEND_STRING("\\\\"); break;
            case '\b': APPEND_STRING("\\b"); break;
            case '\f': APPEND_STRING("\\f"); break;
            case '\n': APPEND_STRING("\\n"); break;
            case '\r': APPEND_STRING("\\r"); break;
            case '\t': APPEND_STRING("\\t"); break;
            case '\x00': APPEND_STRING("\\u0000"); break;
            case '\x01': APPEND_STRING("\\u0001"); break;
            case '\x02': APPEND_STRING("\\u0002"); break;
            case '\x03': APPEND_STRING("\\u0003"); break;
            case '\x04': APPEND_STRING("\\u0004"); break;
            case '\x05': APPEND_STRING("\\u0005"); break;
            case '\x06': APPEND_STRING("\\u0006"); break;
            case '\x07': APPEND_STRING("\\u0007"); break;
            /* '\x08' duplicate: '\b' */
            /* '\x09' duplicate: '\t' */
            /* '\x0a' duplicate: '\n' */
            case '\x0b': APPEND_STRING("\\u000b"); break;
            /* '\x0c' duplicate: '\f' */
            /* '\x0d' duplicate: '\r' */
            case '\x0e': APPEND_STRING("\\u000e"); break;
            case '\x0f': APPEND_STRING("\\u000f"); break;
            case '\x10': APPEND_STRING("\\u0010"); break;
            case '\x11': APPEND_STRING("\\u0011"); break;
            case '\x12': APPEND_STRING("\\u0012"); break;
            case '\x13': APPEND_STRING("\\u0013"); break;
            case '\x14': APPEND_STRING("\\u0014"); break;
            case '\x15': APPEND_STRING("\\u0015"); break;
            case '\x16': APPEND_STRING("\\u0016"); break;
            case '\x17': APPEND_STRING("\\u0017"); break;
            case '\x18': APPEND_STRING("\\u0018"); break;
            case '\x19': APPEND_STRING("\\u0019"); break;
            case '\x1a': APPEND_STRING("\\u001a"); break;
            case '\x1b': APPEND_STRING("\\u001b"); break;
            case '\x1c': APPEND_STRING("\\u001c"); break;
            case '\x1d': APPEND_STRING("\\u001d"); break;
            case '\x1e': APPEND_STRING("\\u001e"); break;
            case '\x1f': APPEND_STRING("\\u001f"); break;
            case '/':
                if (parson_escape_slashes) {
                    APPEND_STRING("\\/");  /* to make json embeddable in xml\/html */
                } else {
                    APPEND_STRING("/");
                }
                break;
            default:
                if (buf != NULL) {
                    buf[0] = c;
                    buf += 1;
                }
                written_total += 1;
                break;
        }
    }
    APPEND_STRING("\"");
    return written_total;
}

/*********************************************************************************************************
** 函数名称: append_indent
** 功能描述: 向指定的缓冲区中追加指定个数的空格块，每个空格块包含 4 个空格字符
** 输	 入: buf - 我们要追加空格块的缓冲区起始地址
**         : level - 我们要追加的空格块个数
** 输	 出: written_total - 一共向 buf 中写入数据的字节数
**		   : -1 - 执行失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static int append_indent(char *buf, int level) {
    int i;
    int written = -1, written_total = 0;
    for (i = 0; i < level; i++) {
        APPEND_STRING("    ");
    }
    return written_total;
}

/*********************************************************************************************************
** 函数名称: append_string
** 功能描述: 把指定的字符串追加到指定的缓冲区中
** 输	 入: buf - 我们要追加字符串的缓冲区起始地址
**         : string - 我们要向缓冲区中追加的字符串内容
** 输	 出: written_total - 一共向 buf 中写入数据的字节数
**		   : -1 - 执行失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
static int append_string(char *buf, const char *string) {
    if (buf == NULL) {
        return (int)strlen(string);
    }
    return sprintf(buf, "%s", string);
}

#undef APPEND_STRING
#undef APPEND_INDENT

/* Parser API */
/*********************************************************************************************************
** 函数名称: json_parse_file
** 功能描述: 读取指定的 JSON 文件内容（序列化格式）并把读取到的“序列化”格式 JSON 数据解析并转换成“树形结构”
**         : JSON 表示格式
** 输	 入: filename - 存储“序列化”格式 JSON 数据的文件名
** 输	 出: output_value - 解析转换后的“树形结构” JSON 数据
**		   : NULL - 执行失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_parse_file(const char *filename) {
    char *file_contents = read_file(filename);
    JSON_Value *output_value = NULL;
    if (file_contents == NULL) {
        return NULL;
    }
    output_value = json_parse_string(file_contents);
    parson_free(file_contents);
    return output_value;
}

/*********************************************************************************************************
** 函数名称: json_parse_file_with_comments
** 功能描述: 读取指定的 JSON 文件内容（序列化格式）并把读取到的“序列化”格式 JSON 数据解析并转换成“树形结构” 
**         : JSON 表示格式
** 注     释: 这个函数除了包含解析 JSON 字符串功能外，还包含去除 JSON 字符串中注释信息的功能逻辑，所以如果在
**         : JSON 文件中不仅包含 JSON 原始数据，还包含了注释信息，那么我们就可以调用这个接口来解析
** 输	 入: filename - 存储“序列化”格式 JSON 数据的文件名
** 输	 出: output_value - 解析转换后的“树形结构” JSON 数据
**		   : NULL - 执行失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_parse_file_with_comments(const char *filename) {
    char *file_contents = read_file(filename);
    JSON_Value *output_value = NULL;
    if (file_contents == NULL) {
        return NULL;
    }
    output_value = json_parse_string_with_comments(file_contents);
    parson_free(file_contents);
    return output_value;
}

/*********************************************************************************************************
** 函数名称: json_parse_string
** 功能描述: 解析指定的 JSON 字符串数据（序列化格式）并转换成能表示 JSON 数据树形结构的表示形式
** 输	 入: string - 序列化格式的 JSON 字符串数据
** 输	 出: JSON_Value - 和序列化 JSON 字符串对应的 JSON 数据结构表示形式的数据
**		   : NULL - 执行失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_parse_string(const char *string) {
    if (string == NULL) {
        return NULL;
    }
    if (string[0] == '\xEF' && string[1] == '\xBB' && string[2] == '\xBF') {
        string = string + 3; /* Support for UTF-8 BOM */
    }
    return parse_value((const char**)&string, 0);
}

/*********************************************************************************************************
** 函数名称: json_parse_string_with_comments
** 功能描述: 解析指定的 JSON 字符串数据（序列化格式）并转换成能表示 JSON 数据树形结构的表示形式
** 注     释: 这个函数除了包含解析 JSON 字符串功能外，还包含去除 JSON 字符串中注释信息的功能逻辑，所以如果
**         : 我们的 JSON 文件中不仅包含 JSON 原始数据，还包含了注释信息，那么我们就可以调用这个接口来解析
** 输	 入: string - 序列化格式的 JSON 字符串数据
** 输	 出: JSON_Value - 和序列化 JSON 字符串对应的 JSON 数据结构表示形式的数据
**		   : NULL - 执行失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_parse_string_with_comments(const char *string) {
    JSON_Value *result = NULL;
    char *string_mutable_copy = NULL, *string_mutable_copy_ptr = NULL;
    string_mutable_copy = parson_strdup(string);
    if (string_mutable_copy == NULL) {
        return NULL;
    }
    remove_comments(string_mutable_copy, "/*", "*/");
    remove_comments(string_mutable_copy, "//", "\n");
    string_mutable_copy_ptr = string_mutable_copy;
    result = parse_value((const char**)&string_mutable_copy_ptr, 0);
    parson_free(string_mutable_copy);
    return result;
}

/* JSON Object API */
/*********************************************************************************************************
** 函数名称: json_object_get_value
** 功能描述: 在指定的 JSON object 中，通过“键值对”中的“键”标识符获取与其对应的“值”标识符的内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
** 输	 出: JSON_Value - 读取到的 JSON_Value 变量
**		   : NULL - 执行失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_object_get_value(const JSON_Object *object, const char *name) {
    if (object == NULL || name == NULL) {
        return NULL;
    }
    return json_object_getn_value(object, name, strlen(name));
}

/*********************************************************************************************************
** 函数名称: json_object_get_string
** 功能描述: 在指定的 JSON object 中，通过 JSONString 类型变量的“键值对”中的“键”标识符获取与其对应的“值”
**         : 标识符的内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
** 输	 出: string - 读取到的 JSONString 类型 string 变量值
**		   : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
const char * json_object_get_string(const JSON_Object *object, const char *name) {
    return json_value_get_string(json_object_get_value(object, name));
}

/*********************************************************************************************************
** 函数名称: json_object_get_number
** 功能描述: 在指定的 JSON object 中，通过 JSONNumber 类型变量的“键值对”中的“键”标识符获取与其对应的“值”
**         : 标识符的内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
** 输	 出: double - 读取到的 JSONNumber 类型 number 变量值
**		   : 0 - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
double json_object_get_number(const JSON_Object *object, const char *name) {
    return json_value_get_number(json_object_get_value(object, name));
}

/*********************************************************************************************************
** 函数名称: json_object_get_object
** 功能描述: 在指定的 JSON object 中，通过 JSONObject 类型变量的“键值对”中的“键”标识符获取与其对应的“值”
**         : 标识符的内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
** 输	 出: JSON_Object - 读取到的 JSONObject 类型 object 变量值
**		   : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Object * json_object_get_object(const JSON_Object *object, const char *name) {
    return json_value_get_object(json_object_get_value(object, name));
}

/*********************************************************************************************************
** 函数名称: json_object_get_array
** 功能描述: 在指定的 JSON object 中，通过 JSONArray 类型变量的“键值对”中的“键”标识符获取与其对应的“值”
**         : 标识符的内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
** 输	 出: JSON_Array - 读取到的 JSONArray 类型 array 变量值
**		   : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Array * json_object_get_array(const JSON_Object *object, const char *name) {
    return json_value_get_array(json_object_get_value(object, name));
}

/*********************************************************************************************************
** 函数名称: json_object_get_boolean
** 功能描述: 在指定的 JSON object 中，通过 JSONBoolean 类型变量的“键值对”中的“键”标识符获取与其对应的“值”
**         : 标识符的内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
** 输	 出: JSONBoolean - 读取到的 JSONBoolean 类型 boolean 变量值
**		   : -1 - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
int json_object_get_boolean(const JSON_Object *object, const char *name) {
    return json_value_get_boolean(json_object_get_value(object, name));
}

/*********************************************************************************************************
** 函数名称: json_object_dotget_value
** 功能描述: 在指定的 JSON object 中，通过“点”描述法获取对应路径位置上的 JSON_Value 变量
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “点”描述法的 JSON_Value 变量路径
** 输	 出: JSON_Value - 读取到的 JSON_Value 变量
**		   : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_object_dotget_value(const JSON_Object *object, const char *name) {
    const char *dot_position = strchr(name, '.');
    if (!dot_position) {
        return json_object_get_value(object, name);
    }
    object = json_value_get_object(json_object_getn_value(object, name, dot_position - name));
    return json_object_dotget_value(object, dot_position + 1);
}

/*********************************************************************************************************
** 函数名称: json_object_dotget_string
** 功能描述: 在指定的 JSON object 中，通过 JSONString 类型变量的“点”描述法获取对应路径位置上的 JSON_Value 变量
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “点”描述法的 JSON_Value 变量路径
** 输	 出: string - 读取到的 JSONString 类型 string 变量值
**		   : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
const char * json_object_dotget_string(const JSON_Object *object, const char *name) {
    return json_value_get_string(json_object_dotget_value(object, name));
}

/*********************************************************************************************************
** 函数名称: json_object_dotget_number
** 功能描述: 在指定的 JSON object 中，通过 JSONNumber 类型变量的“点”描述法获取对应路径位置上的 JSON_Value 变量
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “点”描述法的 JSON_Value 变量路径
** 输	 出: double - 读取到的 JSONNumber 类型 number 变量值
**		   : 0 - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
double json_object_dotget_number(const JSON_Object *object, const char *name) {
    return json_value_get_number(json_object_dotget_value(object, name));
}

/*********************************************************************************************************
** 函数名称: json_object_dotget_object
** 功能描述: 在指定的 JSON object 中，通过 JSONObject 类型变量的“点”描述法获取对应路径位置上的 JSON_Value 变量
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “点”描述法的 JSON_Value 变量路径
** 输	 出: JSON_Object - 读取到的 JSONObject 类型 object 变量值
**		   : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Object * json_object_dotget_object(const JSON_Object *object, const char *name) {
    return json_value_get_object(json_object_dotget_value(object, name));
}

/*********************************************************************************************************
** 函数名称: json_object_dotget_array
** 功能描述: 在指定的 JSON object 中，通过 JSONArray 类型变量的“点”描述法获取对应路径位置上的 JSON_Value 变量
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “点”描述法的 JSON_Value 变量路径
** 输	 出: JSON_Array - 读取到的 JSONArray 类型 array 变量值
**		   : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Array * json_object_dotget_array(const JSON_Object *object, const char *name) {
    return json_value_get_array(json_object_dotget_value(object, name));
}

/*********************************************************************************************************
** 函数名称: json_object_dotget_boolean
** 功能描述: 在指定的 JSON object 中，通过 JSONBoolean 类型变量的“点”描述法获取对应路径位置上的 JSON_Value 变量
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “点”描述法的 JSON_Value 变量路径
** 输	 出: JSONBoolean - 读取到的 JSONBoolean 类型 boolean 变量值
**		   : -1 - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
int json_object_dotget_boolean(const JSON_Object *object, const char *name) {
    return json_value_get_boolean(json_object_dotget_value(object, name));
}

/*********************************************************************************************************
** 函数名称: json_object_get_count
** 功能描述: 获取指定 JSON object 中已经存储的“键值对”个数
** 输	 入: object - 我们要操作的 JSON object 对象
** 输	 出: size_t - “键值对”个数
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
size_t json_object_get_count(const JSON_Object *object) {
    return object ? object->count : 0;
}

/*********************************************************************************************************
** 函数名称: json_object_get_name
** 功能描述: 获取指定 JSON object 中指定索引位置的“键值对”的“键”标识符内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : index - “键”标识符索引值
** 输	 出: string - “键”标识符内容
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
const char * json_object_get_name(const JSON_Object *object, size_t index) {
    if (object == NULL || index >= json_object_get_count(object)) {
        return NULL;
    }
    return object->names[index];
}

/*********************************************************************************************************
** 函数名称: json_object_get_value_at
** 功能描述: 获取指定 JSON object 中指定索引位置的“键值对”的“值”标识符内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : index - “值”标识符索引值
** 输	 出: string - “值”标识符内容
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_object_get_value_at(const JSON_Object *object, size_t index) {
    if (object == NULL || index >= json_object_get_count(object)) {
        return NULL;
    }
    return object->values[index];
}

/*********************************************************************************************************
** 函数名称: json_object_get_wrapping_value
** 功能描述: 获取指定 JSON object 所属的 JSON_Value 的指针
** 输	 入: object - 我们要操作的 JSON object 对象
** 输	 出: JSON_Value - 指定 JSON object 所属的 JSON_Value 的指针
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value *json_object_get_wrapping_value(const JSON_Object *object) {
    return object->wrapping_value;
}

/*********************************************************************************************************
** 函数名称: json_object_has_value
** 功能描述: 判断指定的 JSON object 中是否存在指定“键”标识符的“键值对”数据内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
** 输	 出: 1 - “键值对”数据存在
**         : 0 - “键值对”数据不存在
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
int json_object_has_value (const JSON_Object *object, const char *name) {
    return json_object_get_value(object, name) != NULL;
}

/*********************************************************************************************************
** 函数名称: json_object_has_value_of_type
** 功能描述: 判断指定的 JSON object 中是否存在指定“键”标识符及 JSON_Value_Type 的“键值对”数据内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “键值对”的“键”标识符
**         : type - “键值对”的“值”标识符数据类型
** 输	 出: 1 - “键值对”数据及类型存在
**         : 0 - “键值对”数据及类型不存在
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
int json_object_has_value_of_type(const JSON_Object *object, const char *name, JSON_Value_Type type) {
    JSON_Value *val = json_object_get_value(object, name);
    return val != NULL && json_value_get_type(val) == type;
}

/*********************************************************************************************************
** 函数名称: json_object_has_value_of_type
** 功能描述: 判断指定的 JSON object 中是否存在“点”描述法指定的“键值对”数据内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “点”描述法的 JSON_Value 变量路径
** 输	 出: JSON_Value - 读取到的 JSON_Value 变量
**		   : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
int json_object_dothas_value (const JSON_Object *object, const char *name) {
    return json_object_dotget_value(object, name) != NULL;
}

/*********************************************************************************************************
** 函数名称: json_object_dothas_value_of_type
** 功能描述: 判断指定的 JSON object 中是否存在“点”描述法及 JSON_Value_Type 指定的“键值对”数据内容
** 输	 入: object - 我们要操作的 JSON object 对象
**         : name - “点”描述法的 JSON_Value 变量路径
**         : type - “键值对”的“值”标识符数据类型
** 输	 出: 1 - “键值对”数据及类型存在
**         : 0 - “键值对”数据及类型不存在
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
int json_object_dothas_value_of_type(const JSON_Object *object, const char *name, JSON_Value_Type type) {
    JSON_Value *val = json_object_dotget_value(object, name);
    return val != NULL && json_value_get_type(val) == type;
}

/* JSON Array API */
/*********************************************************************************************************
** 函数名称: json_array_get_value
** 功能描述: 获取指定的 JSON array 中指定索引位置处的 JSON_Value 成员数据内容
** 输	 入: array - 我们要操作的 JSON array 对象
**         : index - 数组元素索引值
** 输	 出: JSON_Value - 读取到的数组成员
**         : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_array_get_value(const JSON_Array *array, size_t index) {
    if (array == NULL || index >= json_array_get_count(array)) {
        return NULL;
    }
    return array->items[index];
}

/*********************************************************************************************************
** 函数名称: json_array_get_string
** 功能描述: 在指定的 JSON array 中，通过 JSONString 类型变量的“索引下标值”获取与其对应的内容
** 输	 入: array - 我们要操作的 JSON array 对象
**         : index - JSONString 类型变量的“索引下标值”
** 输	 出: JSON_Value - 读取到的 JSONString 类型 string 变量值
**         : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
const char * json_array_get_string(const JSON_Array *array, size_t index) {
    return json_value_get_string(json_array_get_value(array, index));
}

/*********************************************************************************************************
** 函数名称: json_array_get_number
** 功能描述: 在指定的 JSON array 中，通过 JSONNumber 类型变量的“索引下标值”获取与其对应的内容
** 输	 入: array - 我们要操作的 JSON array 对象
**         : index - JSONNumber 类型变量的“索引下标值”
** 输	 出: double - 读取到的 JSONNumber 类型 number 变量值
**         : 0 - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
double json_array_get_number(const JSON_Array *array, size_t index) {
    return json_value_get_number(json_array_get_value(array, index));
}

/*********************************************************************************************************
** 函数名称: json_array_get_object
** 功能描述: 在指定的 JSON array 中，通过 JSONObject 类型变量的“索引下标值”获取与其对应的内容
** 输	 入: array - 我们要操作的 JSON array 对象
**         : index - JSONObject 类型变量的“索引下标值”
** 输	 出: double - 读取到的 JSONObject 类型 object 变量值
**         : 0 - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Object * json_array_get_object(const JSON_Array *array, size_t index) {
    return json_value_get_object(json_array_get_value(array, index));
}

/*********************************************************************************************************
** 函数名称: json_array_get_array
** 功能描述: 在指定的 JSON array 中，通过 JSONArray 类型变量的“索引下标值”获取与其对应的内容
** 输	 入: array - 我们要操作的 JSON array 对象
**         : index - JSONArray 类型变量的“索引下标值”
** 输	 出: double - 读取到的 JSONArray 类型 array 变量值
**         : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Array * json_array_get_array(const JSON_Array *array, size_t index) {
    return json_value_get_array(json_array_get_value(array, index));
}

/*********************************************************************************************************
** 函数名称: json_array_get_boolean
** 功能描述: 在指定的 JSON array 中，通过 JSONBoolean 类型变量的“索引下标值”获取与其对应的内容
** 输	 入: array - 我们要操作的 JSON array 对象
**         : index - JSONBoolean 类型变量的“索引下标值”
** 输	 出: double - 读取到的 JSONBoolean 类型 boolean 变量值
**         : -1 - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
int json_array_get_boolean(const JSON_Array *array, size_t index) {
    return json_value_get_boolean(json_array_get_value(array, index));
}

/*********************************************************************************************************
** 函数名称: json_array_get_count
** 功能描述: 获取指定的 JSON_Array 中已经存储的 JSON_Value 成员个数
** 输	 入: array - 我们要操作的 JSON array 对象
** 输	 出: size_t - JSON_Array 成员个数
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
size_t json_array_get_count(const JSON_Array *array) {
    return array ? array->count : 0;
}

/*********************************************************************************************************
** 函数名称: json_array_get_wrapping_value
** 功能描述: 获取指定的 JSON_Array 所属 JSON_Value 的指针
** 输	 入: array - 我们要操作的 JSON array 对象
** 输	 出: JSON_Value - JSON_Array 所属 JSON_Value 的指针
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_array_get_wrapping_value(const JSON_Array *array) {
    return array->wrapping_value;
}

/* JSON Value API */
/*********************************************************************************************************
** 函数名称: json_value_get_type
** 功能描述: 获取指定 JSON_Value 对应的变量类型
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: JSON_Value_Type - JSON_Value 对象类型
**         : JSONError - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value_Type json_value_get_type(const JSON_Value *value) {
    return value ? value->type : JSONError;
}

/*********************************************************************************************************
** 函数名称: json_value_get_object
** 功能描述: 获取指定的 JSONObject 类型的 JSON_Value 所对应的变量值
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: JSON_Object - JSON_Object 变量值
**         : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Object * json_value_get_object(const JSON_Value *value) {
    return json_value_get_type(value) == JSONObject ? value->value.object : NULL;
}

/*********************************************************************************************************
** 函数名称: json_value_get_array
** 功能描述: 获取指定的 JSONArray 类型的 JSON_Value 所对应的变量值
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: JSON_Array - JSONArray 变量值
**         : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Array * json_value_get_array(const JSON_Value *value) {
    return json_value_get_type(value) == JSONArray ? value->value.array : NULL;
}

/*********************************************************************************************************
** 函数名称: json_value_get_string
** 功能描述: 获取指定的 JSONString 类型的 JSON_Value 所对应的变量值
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: string - JSONString 变量值
**         : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
const char * json_value_get_string(const JSON_Value *value) {
    return json_value_get_type(value) == JSONString ? value->value.string : NULL;
}

/*********************************************************************************************************
** 函数名称: json_value_get_number
** 功能描述: 获取指定的 JSONNumber 类型的 JSON_Value 所对应的变量值
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: double - JSONNumber 变量值
**         : 0 - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
double json_value_get_number(const JSON_Value *value) {
    return json_value_get_type(value) == JSONNumber ? value->value.number : 0;
}

/*********************************************************************************************************
** 函数名称: json_value_get_boolean
** 功能描述: 获取指定的 JSONBoolean 类型的 JSON_Value 所对应的变量值
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: JSONBoolean - JSONBoolean 变量值
**         : -1 - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
int json_value_get_boolean(const JSON_Value *value) {
    return json_value_get_type(value) == JSONBoolean ? value->value.boolean : -1;
}

/*********************************************************************************************************
** 函数名称: json_value_get_parent
** 功能描述: 获取指定 JSON_Value 在“树形结构”表示形式中父节点的指针
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: JSON_Value - 父节点的指针
**         : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_value_get_parent (const JSON_Value *value) {
    return value ? value->parent : NULL;
}

/*********************************************************************************************************
** 函数名称: json_value_free
** 功能描述: 释放 JSON “键值对”中的“值”标识符占用的内存资源
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: 
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
void json_value_free(JSON_Value *value) {
    switch (json_value_get_type(value)) {
        case JSONObject:
            json_object_free(value->value.object);
            break;
        case JSONString:
            parson_free(value->value.string);
            break;
        case JSONArray:
            json_array_free(value->value.array);
            break;
        default:
            break;
    }
    parson_free(value);
}

/*********************************************************************************************************
** 函数名称: json_value_init_object
** 功能描述: 创建并初始化一个 JSON_Object 类型的 JSON_Value 变量
** 输	 入: 
** 输	 出: JSON_Value - 创建的 JSON_Object 变量
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_value_init_object(void) {
    JSON_Value *new_value = (JSON_Value*)parson_malloc(sizeof(JSON_Value));
    if (!new_value) {
        return NULL;
    }
    new_value->parent = NULL;
    new_value->type = JSONObject;
    new_value->value.object = json_object_init(new_value);
    if (!new_value->value.object) {
        parson_free(new_value);
        return NULL;
    }
    return new_value;
}

/*********************************************************************************************************
** 函数名称: json_value_init_array
** 功能描述: 创建并初始化一个 JSON_Array 类型的 JSON_Value 变量
** 输	 入: 
** 输	 出: JSON_Value - 创建的 JSON_Array 变量
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_value_init_array(void) {
    JSON_Value *new_value = (JSON_Value*)parson_malloc(sizeof(JSON_Value));
    if (!new_value) {
        return NULL;
    }
    new_value->parent = NULL;
    new_value->type = JSONArray;
    new_value->value.array = json_array_init(new_value);
    if (!new_value->value.array) {
        parson_free(new_value);
        return NULL;
    }
    return new_value;
}

/*********************************************************************************************************
** 函数名称: json_value_init_string
** 功能描述: 创建并初始化一个 JSON_String 类型的 JSON_Value 变量
** 输	 入: string - JSON_String 类型的 JSON_Value 变量初始值
** 输	 出: JSON_Value - 创建的 JSON_String 变量
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_value_init_string(const char *string) {
    char *copy = NULL;
    JSON_Value *value;
    size_t string_len = 0;
    if (string == NULL) {
        return NULL;
    }
    string_len = strlen(string);
    if (!is_valid_utf8(string, string_len)) {
        return NULL;
    }
    copy = parson_strndup(string, string_len);
    if (copy == NULL) {
        return NULL;
    }
    value = json_value_init_string_no_copy(copy);
    if (value == NULL) {
        parson_free(copy);
    }
    return value;
}

/*********************************************************************************************************
** 函数名称: json_value_init_number
** 功能描述: 创建并初始化一个 JSON_Number 类型的 JSON_Value 变量
** 输	 入: number - JSON_Number 类型的 JSON_Value 变量初始值
** 输	 出: JSON_Value - 创建的 JSON_Number 变量
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_value_init_number(double number) {
    JSON_Value *new_value = NULL;
    if (IS_NUMBER_INVALID(number)) {
        return NULL;
    }
    new_value = (JSON_Value*)parson_malloc(sizeof(JSON_Value));
    if (new_value == NULL) {
        return NULL;
    }
    new_value->parent = NULL;
    new_value->type = JSONNumber;
    new_value->value.number = number;
    return new_value;
}

/*********************************************************************************************************
** 函数名称: json_value_init_boolean
** 功能描述: 创建并初始化一个 JSON_Bool 类型的 JSON_Value 变量
** 输	 入: boolean - JSON_Bool 类型的 JSON_Value 变量初始值
** 输	 出: JSON_Value - 创建的 JSON_Bool 变量
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_value_init_boolean(int boolean) {
    JSON_Value *new_value = (JSON_Value*)parson_malloc(sizeof(JSON_Value));
    if (!new_value) {
        return NULL;
    }
    new_value->parent = NULL;
    new_value->type = JSONBoolean;
    new_value->value.boolean = boolean ? 1 : 0;
    return new_value;
}

/*********************************************************************************************************
** 函数名称: json_value_init_null
** 功能描述: 创建并初始化一个 JSON_Null 类型的 JSON_Value 变量
** 输	 入: boolean - JSON_Bool 类型的 JSON_Value 变量初始值
** 输	 出: JSON_Value - 创建的 JSON_Null 变量
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_value_init_null(void) {
    JSON_Value *new_value = (JSON_Value*)parson_malloc(sizeof(JSON_Value));
    if (!new_value) {
        return NULL;
    }
    new_value->parent = NULL;
    new_value->type = JSONNull;
    return new_value;
}

/*********************************************************************************************************
** 函数名称: json_value_deep_copy
** 功能描述: 分别递归遍历指定的“树形结构” JSON 数据，为每一个节点对象分配新的内存空间并复制对应的数据内容
**         : 到新空间中，然后返回“新构建”数的根节点 JSON_Value 地址
** 输	 入: value - 需要被复制的树形结构 JSON 数据
** 输	 出: JSON_Value - 新复制的树形结构 JSON 数据
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value * json_value_deep_copy(const JSON_Value *value) {
    size_t i = 0;
    JSON_Value *return_value = NULL, *temp_value_copy = NULL, *temp_value = NULL;
    const char *temp_string = NULL, *temp_key = NULL;
    char *temp_string_copy = NULL;
    JSON_Array *temp_array = NULL, *temp_array_copy = NULL;
    JSON_Object *temp_object = NULL, *temp_object_copy = NULL;

    switch (json_value_get_type(value)) {
        case JSONArray:
            temp_array = json_value_get_array(value);
            return_value = json_value_init_array();
            if (return_value == NULL) {
                return NULL;
            }
            temp_array_copy = json_value_get_array(return_value);

			// 分别遍历 JSON 数组结构中的每一个成员，并复制到新分配的内存空间中
			// 如果数组当前 JSON 树形结构深度不是一，则会执行递归拷贝，复制整个
			// JSON 树中的所有数据到新分配的对应空间中
            for (i = 0; i < json_array_get_count(temp_array); i++) {
                temp_value = json_array_get_value(temp_array, i);
                temp_value_copy = json_value_deep_copy(temp_value);
                if (temp_value_copy == NULL) {
                    json_value_free(return_value);
                    return NULL;
                }
                if (json_array_add(temp_array_copy, temp_value_copy) == JSONFailure) {
                    json_value_free(return_value);
                    json_value_free(temp_value_copy);
                    return NULL;
                }
            }
            return return_value;
        case JSONObject:
            temp_object = json_value_get_object(value);
            return_value = json_value_init_object();
            if (return_value == NULL) {
                return NULL;
            }

			// 分别遍历 JSON 对象结构中的每一个成员，并复制到新分配的内存空间中
			// 如果数组当前 JSON 树形结构深度不是一，则会执行递归拷贝，复制整个
			// JSON 树中的所有数据到新分配的对应空间中
            temp_object_copy = json_value_get_object(return_value);
            for (i = 0; i < json_object_get_count(temp_object); i++) {
                temp_key = json_object_get_name(temp_object, i);
                temp_value = json_object_get_value(temp_object, temp_key);
                temp_value_copy = json_value_deep_copy(temp_value);
                if (temp_value_copy == NULL) {
                    json_value_free(return_value);
                    return NULL;
                }
                if (json_object_add(temp_object_copy, temp_key, temp_value_copy) == JSONFailure) {
                    json_value_free(return_value);
                    json_value_free(temp_value_copy);
                    return NULL;
                }
            }
            return return_value;
        case JSONBoolean:
            return json_value_init_boolean(json_value_get_boolean(value));
        case JSONNumber:
            return json_value_init_number(json_value_get_number(value));
        case JSONString:
            temp_string = json_value_get_string(value);
            if (temp_string == NULL) {
                return NULL;
            }
            temp_string_copy = parson_strdup(temp_string);
            if (temp_string_copy == NULL) {
                return NULL;
            }
            return_value = json_value_init_string_no_copy(temp_string_copy);
            if (return_value == NULL) {
                parson_free(temp_string_copy);
            }
            return return_value;
        case JSONNull:
            return json_value_init_null();
        case JSONError:
            return NULL;
        default:
            return NULL;
    }
}

/*********************************************************************************************************
** 函数名称: json_serialization_size
** 功能描述: 计算对指定的“树形结构” JSON 数据“序列化”后的字符串所占用的内存空间大小
** 注     释: 这个接口计算的是没有添加格式化空格来提高可阅读性时所占用的内存空间大小
** 输	 入: value - 需要被转换的“树形结构” JSON 数据
** 输	 出: size_t - 序列化后所需要的内存空间大小
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
size_t json_serialization_size(const JSON_Value *value) {
    char num_buf[NUM_BUF_SIZE]; /* recursively allocating buffer on stack is a bad idea, so let's do it only once */
    int res = json_serialize_to_buffer_r(value, NULL, 0, 0, num_buf);
    return res < 0 ? 0 : (size_t)(res + 1);
}

/*********************************************************************************************************
** 函数名称: json_serialize_to_buffer
** 功能描述: 把指定的 JSON 数据“树形结构”的表示形式数据转换成与其对应的“序列化”格式的字符串数据并把转换后的
**         : 结果存储到我们指定的缓存空间中
** 注     释: 这个转换接口没有添加格式化空格来提高可阅读性，所以占用空间比较少
** 输	 入: value - 需要转换的“树形结构” JSON 数据
**         : buf - 用来存储转换后的、“序列化”格式的字符串缓冲区
** 输	 出: buf_size_in_bytes - 用来存储“序列化”字符串的缓冲区空间大小
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_serialize_to_buffer(const JSON_Value *value, char *buf, size_t buf_size_in_bytes) {
    int written = -1;
    size_t needed_size_in_bytes = json_serialization_size(value);
    if (needed_size_in_bytes == 0 || buf_size_in_bytes < needed_size_in_bytes) {
        return JSONFailure;
    }
    written = json_serialize_to_buffer_r(value, buf, 0, 0, NULL);
    if (written < 0) {
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_serialize_to_file
** 功能描述: 把指定的 JSON 数据“树形结构”的表示形式数据转换成与其对应的“序列化”格式的字符串数据并把转换后的
**         : 结果存储到我们指定的文件中
** 注     释: 这个转换接口没有添加格式化空格来提高可阅读性，所以占用空间比较少
** 输	 入: value - 需要转换的“树形结构” JSON 数据
**         : filename - 用来存储转换后的、“序列化”格式的字符串文件名
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_serialize_to_file(const JSON_Value *value, const char *filename) {
    JSON_Status return_code = JSONSuccess;
    FILE *fp = NULL;
    char *serialized_string = json_serialize_to_string(value);
    if (serialized_string == NULL) {
        return JSONFailure;
    }
    fp = fopen(filename, "w");
    if (fp == NULL) {
        json_free_serialized_string(serialized_string);
        return JSONFailure;
    }
    if (fputs(serialized_string, fp) == EOF) {
        return_code = JSONFailure;
    }
    if (fclose(fp) == EOF) {
        return_code = JSONFailure;
    }
    json_free_serialized_string(serialized_string);
    return return_code;
}

/*********************************************************************************************************
** 函数名称: json_serialize_to_string
** 功能描述: 把指定的 JSON 数据“树形结构”的表示形式数据转换成与其对应的“序列化”格式的字符串数据，并返回字
**         : 符串地址
** 注     释: 这个转换接口没有添加格式化空格来提高可阅读性，所以占用空间比较少
** 输	 入: value - 需要转换的“树形结构” JSON 数据
** 输	 出: string - 序列化后字符串指针
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
char * json_serialize_to_string(const JSON_Value *value) {
    JSON_Status serialization_result = JSONFailure;
    size_t buf_size_bytes = json_serialization_size(value);
    char *buf = NULL;
    if (buf_size_bytes == 0) {
        return NULL;
    }
    buf = (char*)parson_malloc(buf_size_bytes);
    if (buf == NULL) {
        return NULL;
    }
    serialization_result = json_serialize_to_buffer(value, buf, buf_size_bytes);
    if (serialization_result == JSONFailure) {
        json_free_serialized_string(buf);
        return NULL;
    }
    return buf;
}

/*********************************************************************************************************
** 函数名称: json_serialization_size_pretty
** 功能描述: 计算如果把指定的 JSON 数据“树形结构”的表示形式数据转换成与其对应的“序列化”格式的字符串数据
**         : 并在“序列化”后的字符串中是否需要添加格式化空格所需要的缓冲区空间大小
** 输	 入: value - 需要转换的“树形结构” JSON 数据
** 输	 出: size_t - 我们序列化指定“树形结构” JSON 数据需要的缓冲区空间大小
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
size_t json_serialization_size_pretty(const JSON_Value *value) {
    char num_buf[NUM_BUF_SIZE]; /* recursively allocating buffer on stack is a bad idea, so let's do it only once */
    int res = json_serialize_to_buffer_r(value, NULL, 0, 1, num_buf);
    return res < 0 ? 0 : (size_t)(res + 1);
}

/*********************************************************************************************************
** 函数名称: json_serialize_to_buffer_pretty
** 功能描述: 把指定的 JSON 数据“树形结构”的表示形式数据转换成与其对应的“序列化”格式的字符串数据，并在
**         : “序列化”字符串时添加格式化空格来提高可阅读性，然后把转换后的结果存储到我们指定的缓存空间中
** 输	 入: value - 需要转换的“树形结构” JSON 数据
**         : buf - 用来存储转换后的、“序列化”格式的字符串缓冲区
**         : buf 缓冲区长度
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_serialize_to_buffer_pretty(const JSON_Value *value, char *buf, size_t buf_size_in_bytes) {
    int written = -1;
    size_t needed_size_in_bytes = json_serialization_size_pretty(value);
    if (needed_size_in_bytes == 0 || buf_size_in_bytes < needed_size_in_bytes) {
        return JSONFailure;
    }
    written = json_serialize_to_buffer_r(value, buf, 0, 1, NULL);
    if (written < 0) {
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_serialize_to_file_pretty
** 功能描述: 把指定的 JSON 数据“树形结构”的表示形式数据转换成与其对应的“序列化”格式的字符串数据，并在
**         : “序列化”字符串时添加格式化空格来提高可阅读性，然后把转换后的结果存储到我们指定的文件中
** 输	 入: value - 需要转换的“树形结构” JSON 数据
**         : filename - 存储“序列化” JSON 的文件名
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_serialize_to_file_pretty(const JSON_Value *value, const char *filename) {
    JSON_Status return_code = JSONSuccess;
    FILE *fp = NULL;
    char *serialized_string = json_serialize_to_string_pretty(value);
    if (serialized_string == NULL) {
        return JSONFailure;
    }
    fp = fopen(filename, "w");
    if (fp == NULL) {
        json_free_serialized_string(serialized_string);
        return JSONFailure;
    }
    if (fputs(serialized_string, fp) == EOF) {
        return_code = JSONFailure;
    }
    if (fclose(fp) == EOF) {
        return_code = JSONFailure;
    }
    json_free_serialized_string(serialized_string);
    return return_code;
}

/*********************************************************************************************************
** 函数名称: json_serialize_to_string_pretty
** 功能描述: 把指定的 JSON 数据“树形结构”的表示形式数据转换成与其对应的“序列化”格式的字符串数据，并在
**         : “序列化”字符串时添加格式化空格来提高可阅读性，然后把转换后的结果存储到我们动态分配的内存
**         : 空间中并返回内存首地址（即字符串地址）
** 输	 入: value - 需要转换的“树形结构” JSON 数据
** 输	 出: string - 序列化后字符串地址
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
char * json_serialize_to_string_pretty(const JSON_Value *value) {
    JSON_Status serialization_result = JSONFailure;
    size_t buf_size_bytes = json_serialization_size_pretty(value);
    char *buf = NULL;
    if (buf_size_bytes == 0) {
        return NULL;
    }
    buf = (char*)parson_malloc(buf_size_bytes);
    if (buf == NULL) {
        return NULL;
    }
    serialization_result = json_serialize_to_buffer_pretty(value, buf, buf_size_bytes);
    if (serialization_result == JSONFailure) {
        json_free_serialized_string(buf);
        return NULL;
    }
    return buf;
}

/*********************************************************************************************************
** 函数名称: json_free_serialized_string
** 功能描述: 释放指定的“序列化” JSON 字符串数据所占用的内存空间
** 输	 入: string - 需要释放的“序列化” JSON 字符串数据
** 输	 出: 
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
void json_free_serialized_string(char *string) {
    parson_free(string);
}

/*********************************************************************************************************
** 函数名称: json_array_remove
** 功能描述: 在“树形结构”中，把指定 JSON_Array 的指定数组索引所对应的成员从数组中移除并释放与其对应的内存空间
** 输	 入: array - 我们要操作的 JSON_Array 对象
**         : ix - 我们要移除并释放的数组成员索引值
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_remove(JSON_Array *array, size_t ix) {
    size_t to_move_bytes = 0;
    if (array == NULL || ix >= json_array_get_count(array)) {
        return JSONFailure;
    }
    json_value_free(json_array_get_value(array, ix));
    to_move_bytes = (json_array_get_count(array) - 1 - ix) * sizeof(JSON_Value*);
    memmove(array->items + ix, array->items + ix + 1, to_move_bytes);
    array->count -= 1;
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_replace_value
** 功能描述: 在“树形结构”中，设置指定 JSON_Array 的指定数组索引所对应成员的数据内容
** 输	 入: array - 我们要操作的 JSON_Array 对象
**         : ix - 我们要修改内容的数组成员索引值
**         : value - 我们要设置的新的数据内容
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_replace_value(JSON_Array *array, size_t ix, JSON_Value *value) {
    if (array == NULL || value == NULL || value->parent != NULL || ix >= json_array_get_count(array)) {
        return JSONFailure;
    }
    json_value_free(json_array_get_value(array, ix));
    value->parent = json_array_get_wrapping_value(array);
    array->items[ix] = value;
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_replace_string
** 功能描述: 在“树形结构”中，设置指定 JSON_Array 的指定数组索引所对应的 JSON_String 类型成员的数据内容
** 输	 入: array - 我们要操作的 JSON_Array 对象
**         : i - 我们要修改内容的数组成员索引值
**         : string - 我们要设置的新的字符串变量内容
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_replace_string(JSON_Array *array, size_t i, const char* string) {
    JSON_Value *value = json_value_init_string(string);
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_array_replace_value(array, i, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_replace_number
** 功能描述: 在“树形结构”中，设置指定 JSON_Array 的指定数组索引所对应的 JSON_Number 类型成员的数据内容
** 输	 入: array - 我们要操作的 JSON_Array 对象
**         : i - 我们要修改内容的数组成员索引值
**         : number - 我们要设置的新的数值变量内容
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_replace_number(JSON_Array *array, size_t i, double number) {
    JSON_Value *value = json_value_init_number(number);
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_array_replace_value(array, i, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_replace_boolean
** 功能描述: 在“树形结构”中，设置指定 JSON_Array 的指定数组索引所对应的 JSON_Bool 类型成员的数据内容
** 输	 入: array - 我们要操作的 JSON_Array 对象
**         : i - 我们要修改内容的数组成员索引值
**         : boolean - 我们要设置的新的布尔变量内容
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_replace_boolean(JSON_Array *array, size_t i, int boolean) {
    JSON_Value *value = json_value_init_boolean(boolean);
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_array_replace_value(array, i, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_replace_null
** 功能描述: 在“树形结构”中，设置指定 JSON_Array 的指定数组索引所对应的 JSON_Null 类型成员的数据内容
** 输	 入: array - 我们要操作的 JSON_Array 对象
**         : i - 我们要修改内容的数组成员索引值
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_replace_null(JSON_Array *array, size_t i) {
    JSON_Value *value = json_value_init_null();
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_array_replace_value(array, i, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_clear
** 功能描述: 清空指定 JSON_Array 对象中所有数组成员内容及释放其占用的内存空间
** 输	 入: array - 我们要操作的 JSON_Array 对象
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_clear(JSON_Array *array) {
    size_t i = 0;
    if (array == NULL) {
        return JSONFailure;
    }
    for (i = 0; i < json_array_get_count(array); i++) {
        json_value_free(json_array_get_value(array, i));
    }
    array->count = 0;
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_append_value
** 功能描述: 向指定的 JSON array 中追加一个新的 JSON_Value 成员
** 输	 入: array - 我们要操作的 JSON_Array 对象
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_append_value(JSON_Array *array, JSON_Value *value) {
    if (array == NULL || value == NULL || value->parent != NULL) {
        return JSONFailure;
    }
    return json_array_add(array, value);
}

/*********************************************************************************************************
** 函数名称: json_array_append_string
** 功能描述: 向指定的 JSON array 中追加一个新的 JSON_String 类型的 JSON_Value 成员
** 输	 入: array - 我们要操作的 JSON_Array 对象
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_append_string(JSON_Array *array, const char *string) {
    JSON_Value *value = json_value_init_string(string);
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_array_append_value(array, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_append_number
** 功能描述: 向指定的 JSON array 中追加一个新的 JSON_Number 类型的 JSON_Value 成员
** 输	 入: array - 我们要操作的 JSON_Array 对象
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_append_number(JSON_Array *array, double number) {
    JSON_Value *value = json_value_init_number(number);
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_array_append_value(array, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_append_boolean
** 功能描述: 向指定的 JSON array 中追加一个新的 JSON_Bool 类型的 JSON_Value 成员
** 输	 入: array - 我们要操作的 JSON_Array 对象
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_append_boolean(JSON_Array *array, int boolean) {
    JSON_Value *value = json_value_init_boolean(boolean);
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_array_append_value(array, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_array_append_null
** 功能描述: 向指定的 JSON array 中追加一个新的 JSON_Null 类型的 JSON_Value 成员
** 输	 入: array - 我们要操作的 JSON_Array 对象
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_array_append_null(JSON_Array *array) {
    JSON_Value *value = json_value_init_null();
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_array_append_value(array, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_object_set_value
** 功能描述: 设置指定 JSON object 中指定“键”描述符所对应的“值”描述符内容，如果指定的 JSON object 中已经
**         : 有了我们指定的“键”描述符所对应的“键值对”，则更新这个“键值对”的内容，如果没有对应的“键值对”
**         : 则向这个 JSON object 中添加一个新的“键值对”
** 输	 入: object - 我们要操作的 JSON object
**         : name - “键值对”的“键”描述符
**         : value - “键值对”的“值”描述符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_set_value(JSON_Object *object, const char *name, JSON_Value *value) {
    size_t i = 0;
    JSON_Value *old_value;
    if (object == NULL || name == NULL || value == NULL || value->parent != NULL) {
        return JSONFailure;
    }
    old_value = json_object_get_value(object, name);
    if (old_value != NULL) { /* free and overwrite old value */
        json_value_free(old_value);
        for (i = 0; i < json_object_get_count(object); i++) {
            if (strcmp(object->names[i], name) == 0) {
                value->parent = json_object_get_wrapping_value(object);
                object->values[i] = value;
                return JSONSuccess;
            }
        }
    }
    /* add new key value pair */
    return json_object_add(object, name, value);
}

/*********************************************************************************************************
** 函数名称: json_object_set_string
** 功能描述: 设置指定 JSON object 中指定 JSON_String 类型“键”描述符所对应的“值”描述符内容，如果指定的
**         : JSON object 中已经有了我们指定的“键”描述符所对应的“键值对”，则更新这个“键值对”的内容，如
**         : 果没有对应的“键值对”，则向这个 JSON object 中添加一个新的 JSON_String 类型“键值对”
** 输	 入: object - 我们要操作的 JSON object
**         : name - “键值对”的“键”描述符
**         : value - “键值对”的“值”描述符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_set_string(JSON_Object *object, const char *name, const char *string) {
    return json_object_set_value(object, name, json_value_init_string(string));
}

/*********************************************************************************************************
** 函数名称: json_object_set_number
** 功能描述: 设置指定 JSON object 中指定 JSON_Number 类型“键”描述符所对应的“值”描述符内容，如果指定的
**         : JSON object 中已经有了我们指定的“键”描述符所对应的“键值对”，则更新这个“键值对”的内容，如
**         : 果没有对应的“键值对”，则向这个 JSON object 中添加一个新的 JSON_Number 类型“键值对”
** 输	 入: object - 我们要操作的 JSON object
**         : name - “键值对”的“键”描述符
**         : value - “键值对”的“值”描述符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_set_number(JSON_Object *object, const char *name, double number) {
    return json_object_set_value(object, name, json_value_init_number(number));
}

/*********************************************************************************************************
** 函数名称: json_object_set_boolean
** 功能描述: 设置指定 JSON object 中指定 JSON_Bool 类型“键”描述符所对应的“值”描述符内容，如果指定的
**         : JSON object 中已经有了我们指定的“键”描述符所对应的“键值对”，则更新这个“键值对”的内容
**         : 如果没有对应的“键值对”，则向这个 JSON object 中添加一个新的 JSON_Bool 类型“键值对”
** 输	 入: object - 我们要操作的 JSON object
**         : name - “键值对”的“键”描述符
**         : value - “键值对”的“值”描述符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_set_boolean(JSON_Object *object, const char *name, int boolean) {
    return json_object_set_value(object, name, json_value_init_boolean(boolean));
}

/*********************************************************************************************************
** 函数名称: json_object_set_null
** 功能描述: 设置指定 JSON object 中指定 JSON_Null 类型“键”描述符所对应的“值”描述符内容，如果指定的
**         : JSON object 中已经有了我们指定的“键”描述符所对应的“键值对”，则更新这个“键值对”的内容
**         : 如果没有对应的“键值对”，则向这个 JSON object 中添加一个新的 JSON_Null 类型“键值对”
** 输	 入: object - 我们要操作的 JSON object
**         : name - “键值对”的“键”描述符
**         : value - “键值对”的“值”描述符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_set_null(JSON_Object *object, const char *name) {
    return json_object_set_value(object, name, json_value_init_null());
}

/*********************************************************************************************************
** 函数名称: json_object_dotset_value
** 功能描述: 设置指定 JSON object 中通过“点”描述法指定“键”描述符所对应的“值”描述符内容，如果指定的 JSON object
**         : 中已经有了我们指定的“键”描述符所对应的“键值对”，则更新这个“键值对”的内容，如果没有对应的“键值对”
**         : 则向这个 JSON object 中添加一个新的“键值对”
** 输	 入: object - 我们要操作的 JSON object
**         : name - “点”描述法指定的“键值对”的“键”描述符
**         : value - “键值对”的“值”描述符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_dotset_value(JSON_Object *object, const char *name, JSON_Value *value) {
    const char *dot_pos = NULL;
    JSON_Value *temp_value = NULL, *new_value = NULL;
    JSON_Object *temp_object = NULL, *new_object = NULL;
    JSON_Status status = JSONFailure;
    size_t name_len = 0;
    if (object == NULL || name == NULL || value == NULL) {
        return JSONFailure;
    }
    dot_pos = strchr(name, '.');
    if (dot_pos == NULL) {
        return json_object_set_value(object, name, value);
    }
    name_len = dot_pos - name;
    temp_value = json_object_getn_value(object, name, name_len);
    if (temp_value) {
        /* Don't overwrite existing non-object (unlike json_object_set_value, but it shouldn't be changed at this point) */
        if (json_value_get_type(temp_value) != JSONObject) {
            return JSONFailure;
        }
        temp_object = json_value_get_object(temp_value);
        return json_object_dotset_value(temp_object, dot_pos + 1, value);
    }
    new_value = json_value_init_object();
    if (new_value == NULL) {
        return JSONFailure;
    }
    new_object = json_value_get_object(new_value);
    status = json_object_dotset_value(new_object, dot_pos + 1, value);
    if (status != JSONSuccess) {
        json_value_free(new_value);
        return JSONFailure;
    }
    status = json_object_addn(object, name, name_len, new_value);
    if (status != JSONSuccess) {
        json_object_dotremove_internal(new_object, dot_pos + 1, 0);
        json_value_free(new_value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_object_dotset_string
** 功能描述: 设置指定 JSON object 中通过“点”描述法指定 JSON_String 类型的“键”描述符所对应的“值”描述符内容
**         : 如果指定的 JSON object 中已经有了我们指定的“键”描述符所对应的“键值对”，则更新这个“键值对”的内
**         : 容，如果没有对应的“键值对”，则向这个 JSON object 中添加一个新的 JSON_String 类型“键值对”
** 输	 入: object - 我们要操作的 JSON object
**         : name - “点”描述法指定的“键值对”的“键”描述符
**         : value - “键值对”的“值”描述符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_dotset_string(JSON_Object *object, const char *name, const char *string) {
    JSON_Value *value = json_value_init_string(string);
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_object_dotset_value(object, name, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_object_dotset_number
** 功能描述: 设置指定 JSON object 中通过“点”描述法指定 JSON_Number 类型的“键”描述符所对应的“值”描述符内容
**         : 如果指定的 JSON object 中已经有了我们指定的“键”描述符所对应的“键值对”，则更新这个“键值对”的内
**         : 容，如果没有对应的“键值对”，则向这个 JSON object 中添加一个新的 JSON_Number 类型“键值对”
** 输	 入: object - 我们要操作的 JSON object
**         : name - “点”描述法指定的“键值对”的“键”描述符
**         : value - “键值对”的“值”描述符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_dotset_number(JSON_Object *object, const char *name, double number) {
    JSON_Value *value = json_value_init_number(number);
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_object_dotset_value(object, name, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_object_dotset_boolean
** 功能描述: 设置指定 JSON object 中通过“点”描述法指定 JSON_Bool 类型的“键”描述符所对应的“值”描述符内容
**         : 如果指定的 JSON object 中已经有了我们指定的“键”描述符所对应的“键值对”，则更新这个“键值对”的内
**         : 容，如果没有对应的“键值对”，则向这个 JSON object 中添加一个新的 JSON_Bool 类型“键值对”
** 输	 入: object - 我们要操作的 JSON object
**         : name - “点”描述法指定的“键值对”的“键”描述符
**         : value - “键值对”的“值”描述符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_dotset_boolean(JSON_Object *object, const char *name, int boolean) {
    JSON_Value *value = json_value_init_boolean(boolean);
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_object_dotset_value(object, name, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_object_dotset_null
** 功能描述: 设置指定 JSON object 中通过“点”描述法指定 JSON_Null 类型的“键”描述符所对应的“值”描述符内容
**         : 如果指定的 JSON object 中已经有了我们指定的“键”描述符所对应的“键值对”，则更新这个“键值对”的内
**         : 容，如果没有对应的“键值对”，则向这个 JSON object 中添加一个新的 JSON_Null 类型“键值对”
** 输	 入: object - 我们要操作的 JSON object
**         : name - “点”描述法指定的“键值对”的“键”描述符
**         : value - “键值对”的“值”描述符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_dotset_null(JSON_Object *object, const char *name) {
    JSON_Value *value = json_value_init_null();
    if (value == NULL) {
        return JSONFailure;
    }
    if (json_object_dotset_value(object, name, value) == JSONFailure) {
        json_value_free(value);
        return JSONFailure;
    }
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_object_remove
** 功能描述: 从指定的 JSON object 中通过“键值对”的“键”标识符找到与其对应的成员并删除和释放“键值对”的“值”
**         : 标识符所占用的资源
** 输	 入: object - 我们要操作的 JSON object
**         : name - “键值对”的“键”标识符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_remove(JSON_Object *object, const char *name) {
    return json_object_remove_internal(object, name, 1);
}

/*********************************************************************************************************
** 函数名称: json_object_dotremove
** 功能描述: 从指定的 JSON object 中通过“点表示法”指定的“键值对”的“键”标识符对应的成员并删除和释放“键值对”
**         : 的“值”标识符所占用的资源
** 输	 入: object - 我们要操作的 JSON object
**         : name - “点表示法”指定的“键值对”的“键”标识符
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_dotremove(JSON_Object *object, const char *name) {
    return json_object_dotremove_internal(object, name, 1);
}

/*********************************************************************************************************
** 函数名称: json_object_clear
** 功能描述: 清空指定 JSON_Object 对象中所有“键值对”成员内容及释放其占用的内存空间
** 输	 入: object - 我们要操作的 JSON object
** 输	 出: JSON_Status - 操作状态
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_object_clear(JSON_Object *object) {
    size_t i = 0;
    if (object == NULL) {
        return JSONFailure;
    }
    for (i = 0; i < json_object_get_count(object); i++) {
        parson_free(object->names[i]);
        json_value_free(object->values[i]);
    }
    object->count = 0;
    return JSONSuccess;
}

/*********************************************************************************************************
** 函数名称: json_validate
** 功能描述: 校验指定的“树形结构” JSON 数据是否符合指定的 JSON 模式，所谓的 JSON 模式是定义了“树形结构” 
**         : JSON 数据需要包含哪些字段以及每个字段的数据类型是什么，即“树形结构” JSON 需要按照什么样的规则
**         : 或表现形式来组织数据，详细介绍查看如下链接：
**         : https://json-schema.org/understanding-json-schema/about.html#about
** 输	 入: schema - JSON 模式数据
**         : value - 需要校验的“树形结构” JSON 数据
** 输	 出: JSON_Status - JSON 数据是否符合 JSON 模式数据
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Status json_validate(const JSON_Value *schema, const JSON_Value *value) {
    JSON_Value *temp_schema_value = NULL, *temp_value = NULL;
    JSON_Array *schema_array = NULL, *value_array = NULL;
    JSON_Object *schema_object = NULL, *value_object = NULL;
    JSON_Value_Type schema_type = JSONError, value_type = JSONError;
    const char *key = NULL;
    size_t i = 0, count = 0;
    if (schema == NULL || value == NULL) {
        return JSONFailure;
    }
    schema_type = json_value_get_type(schema);
    value_type = json_value_get_type(value);
    if (schema_type != value_type && schema_type != JSONNull) { /* null represents all values */
        return JSONFailure;
    }
    switch (schema_type) {
        case JSONArray:
            schema_array = json_value_get_array(schema);
            value_array = json_value_get_array(value);
            count = json_array_get_count(schema_array);
            if (count == 0) {
                return JSONSuccess; /* Empty array allows all types */
            }
            /* Get first value from array, rest is ignored */
            temp_schema_value = json_array_get_value(schema_array, 0);
            for (i = 0; i < json_array_get_count(value_array); i++) {
                temp_value = json_array_get_value(value_array, i);
                if (json_validate(temp_schema_value, temp_value) == JSONFailure) {
                    return JSONFailure;
                }
            }
            return JSONSuccess;
        case JSONObject:
            schema_object = json_value_get_object(schema);
            value_object = json_value_get_object(value);
            count = json_object_get_count(schema_object);
            if (count == 0) {
                return JSONSuccess; /* Empty object allows all objects */
            } else if (json_object_get_count(value_object) < count) {
                return JSONFailure; /* Tested object mustn't have less name-value pairs than schema */
            }
            for (i = 0; i < count; i++) {
                key = json_object_get_name(schema_object, i);
                temp_schema_value = json_object_get_value(schema_object, key);
                temp_value = json_object_get_value(value_object, key);
                if (temp_value == NULL) {
                    return JSONFailure;
                }
                if (json_validate(temp_schema_value, temp_value) == JSONFailure) {
                    return JSONFailure;
                }
            }
            return JSONSuccess;
        case JSONString: case JSONNumber: case JSONBoolean: case JSONNull:
            return JSONSuccess; /* equality already tested before switch */
        case JSONError: default:
            return JSONFailure;
    }
}

/*********************************************************************************************************
** 函数名称: json_value_equals
** 功能描述: 比较指定的两个“树形结构” JSON 数据是否相等并返回比较结果
** 输	 入: a 和 b - 需要比较的两个“树形结构” JSON 数据
** 输	 出: 1 - 相等
**         : 0 - 不相等
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
int json_value_equals(const JSON_Value *a, const JSON_Value *b) {
    JSON_Object *a_object = NULL, *b_object = NULL;
    JSON_Array *a_array = NULL, *b_array = NULL;
    const char *a_string = NULL, *b_string = NULL;
    const char *key = NULL;
    size_t a_count = 0, b_count = 0, i = 0;
    JSON_Value_Type a_type, b_type;
    a_type = json_value_get_type(a);
    b_type = json_value_get_type(b);
    if (a_type != b_type) {
        return 0;
    }
    switch (a_type) {
        case JSONArray:
            a_array = json_value_get_array(a);
            b_array = json_value_get_array(b);
            a_count = json_array_get_count(a_array);
            b_count = json_array_get_count(b_array);
            if (a_count != b_count) {
                return 0;
            }
            for (i = 0; i < a_count; i++) {
                if (!json_value_equals(json_array_get_value(a_array, i),
                                       json_array_get_value(b_array, i))) {
                    return 0;
                }
            }
            return 1;
        case JSONObject:
            a_object = json_value_get_object(a);
            b_object = json_value_get_object(b);
            a_count = json_object_get_count(a_object);
            b_count = json_object_get_count(b_object);
            if (a_count != b_count) {
                return 0;
            }
            for (i = 0; i < a_count; i++) {
                key = json_object_get_name(a_object, i);
                if (!json_value_equals(json_object_get_value(a_object, key),
                                       json_object_get_value(b_object, key))) {
                    return 0;
                }
            }
            return 1;
        case JSONString:
            a_string = json_value_get_string(a);
            b_string = json_value_get_string(b);
            if (a_string == NULL || b_string == NULL) {
                return 0; /* shouldn't happen */
            }
            return strcmp(a_string, b_string) == 0;
        case JSONBoolean:
            return json_value_get_boolean(a) == json_value_get_boolean(b);
        case JSONNumber:
            return fabs(json_value_get_number(a) - json_value_get_number(b)) < 0.000001; /* EPSILON */
        case JSONError:
            return 1;
        case JSONNull:
            return 1;
        default:
            return 1;
    }
}

/*********************************************************************************************************
** 函数名称: json_type
** 功能描述: 获取指定 JSON_Value 对应的变量类型
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: JSON_Value_Type - JSON_Value 对象类型
**         : JSONError - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Value_Type json_type(const JSON_Value *value) {
    return json_value_get_type(value);
}

/*********************************************************************************************************
** 函数名称: json_object
** 功能描述: 获取指定的 JSONObject 类型的 JSON_Value 所对应的变量值
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: JSON_Object - JSON_Object 变量值
**         : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Object * json_object (const JSON_Value *value) {
    return json_value_get_object(value);
}

/*********************************************************************************************************
** 函数名称: json_array
** 功能描述: 获取指定的 JSONArray 类型的 JSON_Value 所对应的变量值
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: JSON_Array - JSONArray 变量值
**         : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
JSON_Array * json_array  (const JSON_Value *value) {
    return json_value_get_array(value);
}

/*********************************************************************************************************
** 函数名称: json_string
** 功能描述: 获取指定的 JSONString 类型的 JSON_Value 所对应的变量值
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: string - JSONString 变量值
**         : NULL - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
const char * json_string (const JSON_Value *value) {
    return json_value_get_string(value);
}

/*********************************************************************************************************
** 函数名称: json_number
** 功能描述: 获取指定的 JSONNumber 类型的 JSON_Value 所对应的变量值
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: double - JSONNumber 变量值
**         : 0 - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
double json_number (const JSON_Value *value) {
    return json_value_get_number(value);
}

/*********************************************************************************************************
** 函数名称: json_boolean
** 功能描述: 获取指定的 JSONBoolean 类型的 JSON_Value 所对应的变量值
** 输	 入: value - 我们要操作的 JSON_Value 对象
** 输	 出: JSONBoolean - JSONBoolean 变量值
**         : -1 - 读取失败
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
int json_boolean(const JSON_Value *value) {
    return json_value_get_boolean(value);
}

/*********************************************************************************************************
** 函数名称: json_set_allocation_functions
** 功能描述: 初始化当前解析 JSON 模块（parson）所使用的动态申请内存函数指针
** 输	 入: malloc_fun - 申请内存函数指针
**         : free_fun - 释放内存函数指针
** 输	 出: 
** 全局变量: 
** 调用模块: 
*********************************************************************************************************/
void json_set_allocation_functions(JSON_Malloc_Function malloc_fun, JSON_Free_Function free_fun) {
    parson_malloc = malloc_fun;
    parson_free = free_fun;
}

void json_set_escape_slashes(int escape_slashes) {
    parson_escape_slashes = escape_slashes;
}
