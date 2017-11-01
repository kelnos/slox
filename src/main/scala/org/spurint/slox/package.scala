package org.spurint

package object slox {
  val MAX_CALL_ARGS = 8

  implicit class EitherRightEnrichments[A, B](val r: Right[A, B]) extends AnyVal {
    def leftCast[C]: Either[C, B] = r.asInstanceOf[Right[C, B]]
  }

  implicit class EitherLeftEnrichments[A, B](val l: Left[A, B]) extends AnyVal {
    def rightCast[D]: Either[A, D] = l.asInstanceOf[Left[A, D]]
  }

  implicit class EitherEnrichments[A, B](val e: Either[A, B]) extends AnyVal {
    def leftMap[C](f: A => C): Either[C, B] = e match {
      case Left(l) => Left(f(l))
      case r @ Right(_) => r.leftCast
    }

    def recoverWith[AA >: A, BB >: B](pf: PartialFunction[A,  Either[AA, BB]]): Either[AA, BB] = e match {
      case Left(l) if pf.isDefinedAt(l) => pf(l)
      case _ => e
    }
  }
}
