typedef struct {
  char * name;
} symbol;

symbol * new_symbol (char * name)
{
  symbol * s = GC_MALLOC(sizeof(symbol));
  s->name = strdup(name);
  return s;
}
