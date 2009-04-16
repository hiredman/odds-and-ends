<?php

class Symbol {
  static $t = null;
  private $name = null;
  public $symbol = true;
  function __construct ($name) {
    $this->name = $name;
  }
  function __toString () {
    return $this->name;
  }
  function lookup_in ($environment) {
    $x = array_reverse($environment);
    foreach($x as $env)
      if (array_key_exists($this."",$env))
        return $env[$this.""];
    if (array_key_exists($this."",Lisp :: $root))
      return Lisp :: $root [$this.""];
    if (function_exists($this.""))
      return $this."";
    return A;
  }
}

class Func {
  private $code = null;
  private $environment = null;
  private $parameters = null;
  function __construct ($parameters, $code, $environment) {
    array_unshift($code,symbol("do"));
    $this->code = $code;
    $this->parameters = $parameters;
    $this->environment = $environment;
  }
  function __toString () {
    return "<#FUNCTION#>";
  }
  function call ($args) {
    if (sizeof($args) != sizeof($this->parameters))
      $x->PARAM_COUNT_MISMATCH();
    $e = $this->environment;
    $frame=array();
    for($i=0;$i<sizeof($args);$i++)
      $frame[$this->parameters[$i].""]=$args[$i];
    array_push($e,$frame);
    return eval1($this->code,$e);
  }
}

function eval1 ($expression, $environment=null) {
  return Lisp :: eval1 ($expression, $environment);
}

function apply1 ($op, $args) {
  if (is_object($op))
    return $op->call($args);
  return call_user_func_array($op,$args);
} 

function str ($form) {
  if (is_array($form)) {
    $buf="";
    foreach($form as $i)
      $buf.=" ".str($i);
    return "(".trim($buf).")";
  } elseif (is_string($form)) {
    return '"'.$form.'"';
  } else {
    return $form."";
  }
}

function pr ($form) {
  print str ($form);
}

function prn ($form) {
  pr ($form);
  print "\n";
}

/****************************************************************************/
class Lisp {
  static $root = null;
  static function init() {
    self :: $root = array();
  }
  static function def ($symbol, $value) {
    self :: $root [$symbol.""] = $value;
    return $symbol;
  }
  static function eval1 ($expression, $environment=null) {
    prn ($expression);
    $environment = ($environment == null) ? array() : $environment;
    if (!is_array ($expression)) {
      if (is_object($expression))
        if ($expression->symbol)
          return $expression->lookup_in($environment);
        else
          return $expression;
      else
        return $expression;
    } else {
      if ($expression[0]."" == "quote") {
        array_shift($expression);
        return $expression;
      } else if ($expression[0]."" == "if") {
        if (self :: eval1($expression[1],$environment) != null)
          return self :: eval1($expression[2],$environment);
        else
          return self :: eval1($expression[3],$environment);
      } else if ($expression[0]."" == "do") {
        $b=null;
        foreach ($expression as $part)
          $b=self :: eval1($part,$environment);
        return $b;
      } else if ($expression[0]."" == "fn*") {
        array_shift($expression);
        $args = array_shift($expression);
        return new Func($args,$expression,$environment);
      } else if ($expression[0]."" == "def") {
        return self :: def ($expression[1], self :: eval1 ($expression[2]));
      } else {
        $b=array();
        foreach ($expression as $part)
          array_push($b, self :: eval1($part, $environment));
        $op = array_shift($b);
        return apply1($op, $b);
      }
    }
    return null;
  }
  static function tok ($string) {
    $buf=array();
    for($i=0;$i<strlen($string);$i++)
      array_push($buf,$string[$i]);
    return $buf;
  }
  static function read1 ($stream) {
    $output=array();
    while(sizeof($stream) > 0) {
      $head = array_shift($stream);
      if (is_numeric($head)) {
        $buf=$head;
        while(sizeof($stream) > 0 && is_numeric($head)) {
          $head = array_shift($stream);
          $buf.=$head;
        }
        array_push($output, intval($buf));
      } else if ($head == "(") {
        $buf=array($head);
        $head=array_shift($stream);
        for ($i=1;$i>0;) {
          if ($head == "(")
            $i++;
          else if ($head == ")")
            $i--;
          array_push($buf,$head);
          $head=array_shift($stream);
        }
        array_shift($buf);
        array_pop($buf);
        array_push($output, self :: read1 ($buf));
      } else if ($head != " ") {
        $buf=$head;
        while(sizeof($stream) > 0 && $head != " ") {
          $head = array_shift ($stream);
          $buf .= ($head != " ") ? $head : "";
        }
        array_push($output, new Symbol($buf));
      }
    }
    return $output;
  }
  static function read ($string) {
    return self :: read1 (self :: tok ($string));
  }
}
Lisp :: init ();
/****************************************************************************/

function symbol ($string) {
  return new Symbol($string);
}

function first ($list) {
  return $list[0][0];
}

function cons ($element, $list) {
  array_unshift($list, $element);
  return  $list;
}

print "<pre>";
?>
