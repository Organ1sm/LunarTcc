#ifndef ERROR_LOGGER_H_
#define ERROR_LOGGER_H_

static void PrintImpl(const char *str, unsigned tab = 0, bool newline = false);

static void Print(const char *str, unsigned tab = 0);

static void PrintLn(const char *str, unsigned tab = 0);

#endif