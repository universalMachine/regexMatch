package com.wang.regex


import org.slf4j.{Logger, LoggerFactory}


package object typedef {
  type State[+A] = List[A]
  type Pos = Int
}

package object helper {
  val logger: Logger = LoggerFactory.getLogger("com.wang.regex");
}
