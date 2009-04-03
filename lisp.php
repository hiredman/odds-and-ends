<?php

class Symbol {
  static $t = null;
  private $name = null;
  function __construct ($name) {
	$this->name = $name;}
  function __toString () {
	return $this->name;}
  function type () {
    $this->t = ($this->t == null) ? new Symbol ("symbol") : $this->t;
    return $this->t;}
  function call ($args) {
    return $this->eval1(null)->call($args);}
  function eval1 ($env) {
    return RT :: resolve ($this, $env);}}


class Map implements ArrayAccess {
  private $keys = null;
  private $values = null;
  function __construct ($keys, $values) {
	$this->keys = $keys;
	$this->values = $values;}
  static function create () {
	return new Map(null, null);}
  function offsetExists ($k) {
	if (array_search ($k, $this->keys) === false)
	  return false;
	return true;}
  function offsetGet ($x) {
	$y = array_search($x, $this->keys);
	if ($y === false)
	  return null;
	else
	  return $this->values[$y];}
  function offsetSet ($k, $v) {}
  function offsetUnset ($k) {}
  function __toString () {
	$buf="";
	if($this->keys!=null)
	  foreach($this->keys as $key => $kval)
		$buf.=" ".$kval." ".$this->values[$key].",";
	$buf=substr($buf,0,strlen($buf)-1);
	return "{".trim($buf)."}";}
  function assoc ($k, $v) {
	$keys = $this->keys;
	$values = $this->values;
	if($keys == null) {
	  $keys = array();
	  $values = array();}
	array_push($keys,$k);
	array_push($values,$v);
	return new Map($keys,$values);}}

class Primitive {
  private $name = null;
  public $macro = false;
  function __toString () {
	return "&lt;PRIMITIVE".$this->name."&gt;";}
  function __construct ($args, $body) {
	$this->name = create_function($args, $body);}
  function eval1 () {
    return $this;}
  function call($args) {
	return call_user_func_array($this->name,$args->toArray());}}

interface Sequence {
  function &first ();
  function Pempty();
  function &rest ();}

class SequenceIterator implements Iterator {
  private $it = null;
  function __construct(&$a) {
	$this->it = new ArrayIterator($a);}
  function current () {
	return $this->it->current();}
  function next () {
	return $this->it->next();}
  function key () {} 
  function valid () {return $this->it->valid();} 
  function rewind () {} 
}

class ArrayList implements Sequence, IteratorAggregate, ArrayAccess {
  protected $array = null;
  public $list = true;
  function __construct () {
	$this->array = func_get_args();}
  function getIterator() {
	return new SequenceIterator($this->array);}
  function fromArray (& $arr) {
	$this->array = &$arr;
	return $this;}
  function toArray () {
	$x = $this->array;
	return $x;}
  function cons ($x) {
	$y = new ArrayList();
	$a = $this->array;
	array_unshift($a,$x);
	$y->fromArray($a);
	return $y;}
  function &first () {
	return $this->array[0];}
  function Pempty () {
	if(sizeof($this->array) == 0)
	  return true;
	else
	  return false;}
  function &rest () {
	$x=$this->array;
	array_shift($x);
	$y=new ArrayList();
	$y->fromArray($x);
	return $y;}
  function __toString () {
	$buf="";
	foreach($this->array as $a)
	  $buf.=" ".$a;
	return "(".trim($buf).")";}
  function reverse () {
    $n = new ArrayList ();
    foreach ($this as $that)
      $n = $n -> cons ($that);
    return $n;}
  function offsetExists ($x) {
	return array_key_exists($x,$this->array);}
  function offsetGet ($x) {
	return $this->array[$x];}
  function offsetSet ($x, $v) {}
  function offsetUnset ($x) {}
  static function n () {
	return new ArrayList();}
  function eval1 ($env) {
    if (RT :: resolve ($this -> first ()) -> macro)
      return $this;
    $n = new ArrayList ();
    foreach ($this as $thing)
      $n = $n -> cons (RT::eval1 ($thing, $env));
    return $n -> reverse ();}}

class ArrayArray {
  function __toString () {
	$buf="";
	foreach($this->array as $a)
	  $buf.=" ".$a;
	return "[".trim($buf)."]";}}


class RT {
  const special_chars = "\"(){}[]";
  static $root = null;
  static $lex = null;
  static function resolve ($symbol, $env=null) {
    if ($env != null)
      foreach ($env as $bindings)
        if ($bindings -> offsetExists ($symbol))
          return $bindings [$symbol];
    foreach (self :: $lex as $bindings)
      if ($bindings -> offsetExists ($symbol))
        return $bindings [$symbol];
    if (self :: $root -> offsetExists ($symbol))
      return self :: $root [$symbol];
    else {
      $x=null;
      return $x->Symbol_Lookup_Failed ();}}
  static function init () {
    self :: $lex = new ArrayList ();
    self :: $root = Map :: create ();}
  static function eval1 ($form, $env) {
    if (is_object($form)) {
      return $form->eval1($env);}
    else {
      return $form;}}
  static function apply1 ($form) {
    if ($form->list)
      return $form -> first () -> call ($form -> rest());
    else
      return $form;}
  static function run ($form) {
    return self :: apply1 (self :: eval1 ($form,null));}
  static function run1 ($form, $env) {
    return self :: apply1 (self :: eval1 ($form,$env));}
  static function def ($symbol, $value) {
    self::$root = self::$root->assoc($symbol,$value);}}

class Fn {
  private $code = null;
  private $env = null;
  private $names = null;
  public $macro = false;
  function __construct ($code,$env,$names) {
    $this->code = $code;
    $this->env = ($env == null) ? new ArrayList () : $env;
    $this->names = $names;}
  function call () {
    $a = func_get_args ();
    $a=$a[0];
    $a=$a->toArray();
    if (sizeof($a) != sizeof($this -> names -> toArray()))
      $a -> Incorrect_function_arity ();
    $m = Map :: create ();
    foreach($a as $key => $value)
      $m = $m -> assoc ($this -> names [$key], $value);
    $stack = $this -> env -> cons ($m);
    $stack - $stack -> cons (Map :: create () -> assoc ("THIS", $stack))
    return RT :: run1 ($this -> code, $stack);}
  function __toString () {
    return "&lt;FUNCTION ".$this->names." ".$this->code." &gt;";}}

class Func extends Fn {
  function __construct ($code,$names,$env=null) {
    $this->code=$code;$this->names=$names;$this->env=new ArrayList();
  }}


print "<pre>";
RT :: init ();
RT :: def (new Symbol ("+"), new Primitive('$a,$b','return $a+$b;'));
RT :: def (new Symbol ("first"), new Primitive('$a','return $a->first();'));
RT :: def (new Symbol ("rest"), new Primitive('$a','return $a->rest();'));
RT :: def (new Symbol ("cons"), new Primitive('$a,$b','$b = ($b == null) ? new ArrayList () : $b; return $b->cons($a);'));
RT :: def (new Symbol ("list"), new Primitive('','$a=func_get_args(); $b = new ArrayList; $b -> fromArray ($a); return $b;'));
RT :: def (new Symbol ("def"), new Primitive('$a,$b','return RT :: def ($a,RT :: run ($b));'));
$x = RT :: resolve (new Symbol ("def"));
$x->macro = true;
$x = new ArrayList(new Symbol ("def"), new Symbol ("tokenize"),
                   new Primitive ('$string', 'return tokenize($string);'));
RT :: run ($x);

function tokenize ($string) {
  $a=array();
  for($i=0;$i<strlen($string);$i++)
    array_push($a,$string[$i]);
  $b=new ArrayList();
  $b->fromArray($a);
  return $b;}

function coaless ($list) {
    $result = array();
    while (!$list->Pempty()) {
      if (is_numeric($list->first())) {
        $buf="";
        while (!$list->Pempty() && is_numeric($list->first())) {
          $buf.=$list->first();
          $list=$list->rest();}
        array_push($result, intval($buf));}
      elseif ($list->first() != " " && (strstr(RT::special_chars, $list->first()) === false)) {
        $buf="";
        while (!$list->Pempty() && $list->first() != " " && (strstr(RT::special_chars, $list->first()) === false)) {
          $buf.=$list->first();
          $list=$list->rest();}
          array_push($result,new Symbol ($buf));}
      elseif ($list->first() == "\"") {
		$buf="";
		$list=$list->rest();
		for(;!$list->Pempty() and $list->first() != "\"";$list=$list->rest())
		  $buf.=$list->first();
        array_push($result, $buf);
	  }
      elseif ($list->first() == "(") {
        $buf=new ArrayList ();
        $list=$list->rest();
        for($i=1;$i>0;$list=$list->rest()) {
          $buf=$buf->cons($list->first());
          if ($list->first() == "(")
            $i++;
          elseif ($list->first() == ")")
            $i--;}
        $buf=$buf->rest();
        array_push($result, coaless($buf->reverse()));}
      else {
        if ($list->first() != " ")
          array_push($result, $list->first());
        $list=$list->rest();}}
    $b=new ArrayList ();
    $b->fromArray($result);
    return $b;}

function read ($string) {
  return coaless(tokenize($string));}

$y = new Primitive('','$a=func_get_args();
                       $arg_list = $a[0];
                       array_shift($a);
                       $b=new ArrayList ();
                       $b->fromArray($a);
                       $b=$b->cons(new Symbol ("do"));
                       return new Fn($b,null,$arg_list);');
$y->macro=true;
RT :: def (new Symbol ("fn"), $y);
RT :: def (new Symbol ("do"), new Primitive('','$a=func_get_args();
                                                $b=null;
                                                foreach($a as $c)
                                                  $b=RT::apply1($c);
                                                return $b;'));

/*  return call_user_func_array(
      array(new ReflectionClass($className), 'newInstance'),
          $functionParameters
            );*/
$x=new Primitive('', '$a=func_get_args();
                      $class=$a[0];
                      array_shift($a);
                      return call_user_func_array(array(new ReflectionClass($class), "newInstance"),$a);
                      ');
$x->macro=true;
   
$x=read("(fn (x y) (+ x y))");
$t=RT::run($x->first());
$x=read("(def add (fn (x y) (+ x y)))");
$t=RT::run($x->first());
$x=read("(add 1 2)")->cons(new Symbol("do"));
print RT :: run ($x)."\n";
$x=read("(def f (fn (x) (fn () x)))");
$x=$x->first();
$t=RT::run($x);
print RT :: run (read ("((f 1))") -> first ());
?>
