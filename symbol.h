void * symbol_to_s (void * thi, void * args)
{
  object * o = thi;
  return o->ref;
}

object * symbol (char * name)
{
  char * foo = GC_MALLOC((strlen(name)+1) * sizeof(char));
  strncat(foo,name,strlen(name));
  object * o = new_object();
  o->type =  "symbol";
  o->ref = foo;
  call(o, "attach_method", new_method(& symbol_to_s, "to_s"));
  return o;
}
