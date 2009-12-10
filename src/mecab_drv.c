#include <string.h>
#include "mecab.h"
#include "erl_driver.h"
#include "erl_interface.h"

#define OP_BEGIN_PARSING 1
#define OP_GET_VERSION   2
#define OP_GET_DIC_INFO  3

#define OK            0
#define CANNOT_MALLOC 1
#define BAD_ARGS      2
#define MECAB_ERROR   3

#define THROW(e) { result = e; break; }

#define MALLOC(var, type, size)               \
  var = (type*)malloc(sizeof(type) * (size)); \
  if (var == NULL) THROW(CANNOT_MALLOC);

#define ATOM       100
#define TUPLE      104
#define EMPTY_LIST 106
#define LIST       107
#define BINARY     109

#define STRING_TYPE(type) (type == ATOM ||       \
                           type == LIST ||       \
                           type == EMPTY_LIST || \
                           type == BINARY)
typedef struct {
  ErlDrvPort port;
} mecab_data;

typedef struct {
  ErlDrvPort port;
  mecab_t* mecab;
  erlang_ref* id;
  char* sentence;
} mecab_async_data;

static void parse(void*);

static void encode_ok(ei_x_buff* x) {
  ei_x_new_with_version(x);
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "ok");
}

static void encode_error(ei_x_buff* x) {
  ei_x_new_with_version(x);
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "error");
}

static void encode_error_by_result(ei_x_buff* x, int result, mecab_t* mecab) {
  encode_error(x);

  switch (result) {
  case CANNOT_MALLOC:
    ei_x_encode_atom(x, "cannot_malloc");
    break;
  case BAD_ARGS:
    ei_x_encode_atom(x, "bad_args");
    break;
  case MECAB_ERROR:
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_atom(x, "mecab_error");
    ei_x_encode_string(x, mecab_strerror(mecab));
    break;
  default:
    ei_x_encode_atom(x, "unknown_error");
    break;
  }
}

static void encode_mecab(ei_x_buff* x, mecab_async_data* a) {
  ei_x_new_with_version(x);
  ei_x_encode_tuple_header(x, 3);
  ei_x_encode_atom(x, "mecab");
  ei_x_encode_ref(x, a->id);
}

static int decode_string(const char* buff, int* index, char* data, int size, int type) {
  int result;

  if (type == ATOM) {
    result = ei_decode_atom(buff, index, data);
  }
  else if (type == LIST || type == EMPTY_LIST) {
    result = ei_decode_string(buff, index, data);
  }
  else {
    result = ei_decode_binary(buff, index, (void*)data, (long*)&size);
    if (!result) data[size] = '\0';
  }

  return result;
}

static void begin_parsing(ei_x_buff* x, const char* buff, const ErlDrvPort port) {
  int index = 1;
  int type;
  int size;
  int arity;
  int i;
  int options_count;
  int result = OK;

  int argc = 0;
  char** argv = NULL;
  int dync = 0;
  char** dynv = NULL;
  char* key = NULL;
  double dvalue;

  mecab_t* mecab = NULL;
  erlang_ref* id = NULL;
  char* sentence = NULL;
  mecab_async_data* a = NULL;

  do {
    if (ei_decode_tuple_header(buff, &index, &arity) || arity != 3)
      THROW(BAD_ARGS);

    MALLOC(id, erlang_ref, 1);
    if (ei_decode_ref(buff, &index, id)) THROW(BAD_ARGS);

    ei_get_type(buff, &index, &type, &size);
    if (!STRING_TYPE(type)) THROW(BAD_ARGS);

    MALLOC(sentence, char, size + 1);
    if (decode_string(buff, &index, sentence, size, type)) THROW(BAD_ARGS);

    if (ei_decode_list_header(buff, &index, &arity)) THROW(BAD_ARGS);
    options_count = arity;

    if (options_count > 0) {
      MALLOC(argv, char*, options_count * 2 + 1);
      MALLOC(dynv, char*, options_count);
      argv[argc++] = "";

      for (i = 0; i < options_count; i++) {
        ei_get_type(buff, &index, &type, &size);

        if (STRING_TYPE(type)) {
          MALLOC(key, char, size + 1);
          if (decode_string(buff, &index, key, size, type)) THROW(BAD_ARGS);

          if (!strcmp(key, "partial")) {
            argv[argc++] = "-p";
          }
          else if (!strcmp(key, "best")) {
            argv[argc++] = "-l";
            argv[argc++] = "0";
          }
          else if (!strcmp(key, "nbest")) {
            argv[argc++] = "-l";
            argv[argc++] = "1";
          }
          else if (!strcmp(key, "softseg")) {
            argv[argc++] = "-l";
            argv[argc++] = "2";
          }
          else if (!strcmp(key, "all_morphs")) {
            argv[argc++] = "-a";
          }
          else {
            THROW(BAD_ARGS);
          }

          free(key);
          key = NULL;
        }
        else if (type == TUPLE && size == 2) {
          if (ei_decode_tuple_header(buff, &index, &arity)) THROW(BAD_ARGS);
          ei_get_type(buff, &index, &type, &size);
          if (!STRING_TYPE(type)) THROW(BAD_ARGS);

          MALLOC(key, char, size + 1);
          if (decode_string(buff, &index, key, size, type)) THROW(BAD_ARGS);

          if (!strcmp(key, "dic")) {
            ei_get_type(buff, &index, &type, &size);

            if (type == ATOM) {
              MALLOC(dynv[dync], char, size + 1);
              dync++;
              if (ei_decode_atom(buff, &index, dynv[dync - 1]) ||
                  strcmp(dynv[dync - 1], "default")) THROW(BAD_ARGS);
            }
            else if (STRING_TYPE(type)) {
              MALLOC(dynv[dync], char, size + 1);
              dync++;
              if (decode_string(buff, &index, dynv[dync - 1], size, type))
                THROW(BAD_ARGS);
              argv[argc++] = "-d";
              argv[argc++] = dynv[dync - 1];
            }
            else {
              THROW(BAD_ARGS);
            }
          }
          else if (!strcmp(key, "theta")) {
            if (ei_decode_double(buff, &index, &dvalue)) THROW(BAD_ARGS);
            MALLOC(dynv[dync], char, 64);
            dync++;
            sprintf(dynv[dync - 1], "%f", dvalue);
            argv[argc++] = "-t";
            argv[argc++] = dynv[dync - 1];
          }
          else {
            THROW(BAD_ARGS);
          }

          free(key);
          key = NULL;
        }
        else {
          THROW(BAD_ARGS);
        }
      }
    }

    mecab = mecab_new(argc, argv);
    if (mecab == NULL) THROW(MECAB_ERROR);

    MALLOC(a, mecab_async_data, 1);
    a->port = port;
    a->mecab = mecab;
    a->id = id;
    a->sentence = sentence;
  } while (0);

  if (result != OK) {
    encode_error_by_result(x, result, mecab);
    mecab_destroy(mecab);
    free(id);
    free(sentence);
    free(a);
    free(key);
    for (i = 0; i < dync; i++) free(dynv[i]);
    free(dynv);
    free(argv);
    return;
  }

  encode_ok(x);
  ei_x_encode_ref(x, id);
  driver_async(port, NULL, parse, a, free);

  free(key);
  for (i = 0; i < dync; i++) free(dynv[i]);
  free(dynv);
  free(argv);
}

static void parse(void* async_data) {
  mecab_async_data* a = (mecab_async_data*)async_data;
  const mecab_node_t* node = mecab_sparse_tonode(a->mecab, a->sentence)->next;
  ei_x_buff x;

  if (node == NULL) {
    encode_mecab(&x, a);
    ei_x_encode_tuple_header(&x, 2);
    ei_x_encode_atom(&x, "error");
    ei_x_encode_string(&x, mecab_strerror(a->mecab));
    driver_output(a->port, x.buff, x.index);
    ei_x_free(&x);
    return;
  }

  for (; node; node = node->next) {
    encode_mecab(&x, a);

    if (node->stat == MECAB_BOS_NODE || node->stat == MECAB_EOS_NODE) {
      ei_x_encode_atom(&x, "bos_eos");
    }
    else {
      ei_x_encode_tuple_header(&x, 2);
      ei_x_encode_atom(&x, "node");
      ei_x_encode_tuple_header(&x, 2);
      ei_x_encode_binary(&x, node->surface, node->length);
      ei_x_encode_binary(&x, node->feature, strlen(node->feature));
    }

    driver_output(a->port, x.buff, x.index);
    ei_x_free(&x);
  }

  encode_mecab(&x, a);
  ei_x_encode_atom(&x, "complete");
  driver_output(a->port, x.buff, x.index);
  ei_x_free(&x);
}

static void get_version(ei_x_buff* x) {
  ei_x_new_with_version(x);
  ei_x_encode_string(x, mecab_version());
}

static void get_dic_info(ei_x_buff* x, const char* buff) {
  const mecab_dictionary_info_t* info;

  int index = 1;
  int type;
  int size;
  int result = OK;

  int argc = 0;
  char** argv = NULL;
  char* dicdir = NULL;
  mecab_t* mecab = NULL;

  do {
    ei_get_type(buff, &index, &type, &size);

    if (type == ATOM) {
      MALLOC(dicdir, char, size + 1);
      if (ei_decode_atom(buff, &index, dicdir) || strcmp(dicdir, "default"))
        THROW(BAD_ARGS);
    }
    else if (STRING_TYPE(type)) {
      MALLOC(dicdir, char, size + 1);
      if (decode_string(buff, &index, dicdir, size, type)) THROW(BAD_ARGS);
    }
    else {
      THROW(BAD_ARGS);
    }

    if (type != ATOM) {
      MALLOC(argv, char*, argc = 3);
      argv[0] = "";
      argv[1] = "-d";
      argv[2] = dicdir;
    }

    mecab = mecab_new(argc, argv);
    if (mecab == NULL) THROW(MECAB_ERROR);
  } while (0);

  if (result != OK) {
    encode_error_by_result(x, result, mecab);
    mecab_destroy(mecab);
    free(dicdir);
    free(argv);
    return;
  }

  info = mecab_dictionary_info(mecab);
  encode_ok(x);
  ei_x_encode_tuple_header(x, 7);
  ei_x_encode_string(x, info->filename);
  ei_x_encode_ulong(x, info->version);
  ei_x_encode_string(x, info->charset);

  switch (info->type) {
  case MECAB_USR_DIC:
    ei_x_encode_atom(x, "usr");
    break;
  case MECAB_SYS_DIC:
    ei_x_encode_atom(x, "sys");
    break;
  case MECAB_UNK_DIC:
    ei_x_encode_atom(x, "unk");
    break;
  default:
    ei_x_encode_atom(x, "unknown");
    break;
  }

  ei_x_encode_ulong(x, info->lsize);
  ei_x_encode_ulong(x, info->rsize);
  ei_x_encode_ulong(x, info->size);

  mecab_destroy(mecab);
  free(dicdir);
  free(argv);
}

static ErlDrvData start(ErlDrvPort port, char* buff) {
  mecab_data* d = (mecab_data*)driver_alloc(sizeof(mecab_data));
  d->port = port;
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData)d;
}

static void stop(ErlDrvData handle) {
  driver_free((void*)handle);
}

static int control(ErlDrvData handle, unsigned int command,
                   char* buff, int len, char** rbuff, int rlen) {
  mecab_data* d = (mecab_data*)handle;
  ErlDrvBinary* bin;
  ei_x_buff x;

  switch (command) {
  case OP_BEGIN_PARSING:
    begin_parsing(&x, buff, d->port);
    break;
  case OP_GET_VERSION:
    get_version(&x);
    break;
  case OP_GET_DIC_INFO:
    get_dic_info(&x, buff);
    break;
  default:
    encode_error(&x);
    ei_x_encode_atom(&x, "missing_op");
    break;
  }

  bin = driver_alloc_binary(x.index);

  if (bin != NULL) {
    memcpy(bin->orig_bytes, x.buff, x.index);
    *rbuff = (char*)bin;
    rlen = bin->orig_size;
  }
  else {
    *rbuff = NULL;
  }

  ei_x_free(&x);
  return rlen;
}

static void ready_async(ErlDrvData handle, ErlDrvThreadData async_data) {
  mecab_async_data* a = (mecab_async_data*)async_data;
  mecab_destroy(a->mecab);
  free(a->id);
  free(a->sentence);
  free(a);
}

ErlDrvEntry mecab_driver_entry = {
  .init            = NULL,
  .start           = start,
  .stop            = stop,
  .output          = NULL,
  .ready_input     = NULL,
  .ready_output    = NULL,
  .driver_name     = "mecab_drv",
  .finish          = NULL,
  .handle          = NULL,
  .control         = control,
  .timeout         = NULL,
  .outputv         = NULL,
  .ready_async     = ready_async,
  .flush           = NULL,
  .call            = NULL,
  .event           = NULL,
  .extended_marker = ERL_DRV_EXTENDED_MARKER,
  .major_version   = ERL_DRV_EXTENDED_MAJOR_VERSION,
  .minor_version   = ERL_DRV_EXTENDED_MINOR_VERSION,
  .driver_flags    = 0,
  .handle2         = NULL,
  .process_exit    = NULL,
  .stop_select     = NULL
};

DRIVER_INIT(mecab_drv) {
  return &mecab_driver_entry;
}
