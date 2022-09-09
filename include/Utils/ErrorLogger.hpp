#ifndef ERROR_LOGGER_H_
#define ERROR_LOGGER_H_

void PrintImpl(const char *str, unsigned tab = 0, bool newline = false);

void Print(const char *str, unsigned tab = 0);

void PrintLn(const char *str, unsigned tab = 0);

#endif