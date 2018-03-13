package org.scalafmt.internal

import org.scalafmt.internal.ScalaToken._

import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc._

import scala.meta.{Term, Tree}

import scala.annotation.tailrec

object LambdaArg {
  import TreePrinter._
  import TreeDocOps._
  type Paramss = Vector[List[Term.Param]]

  @tailrec
  private final def getParamss(
      f: Term.Function,
      accum: Paramss = Vector.empty
  ): (Paramss, Term) =
    f.body match {
      case g: Term.Function =>
        getParamss(g, accum :+ f.params)
      case Term.Block((g: Term.Function) :: Nil) =>
        getParamss(g, accum :+ f.params)
      case _ =>
        (accum :+ f.params, f.body)
    }

  def dFunction(f: Term.Function): Doc = {
    val (paramss, body) = getParamss(f)
    val dbody = body match {
      case Term.Block(stats) => dStats(stats)
      case _ => print(body)
    }
    val dparamss = paramss.foldLeft(empty) {
      case (accum, params) =>
        accum + line + dParams(params, forceParens = false) + space + `=>`
    }

    val function =
      (
        dparamss.nested(2).grouped + line +
          dbody
      ).nested(2).grouped

    (`{` + function + line + `}`).grouped
  }

  def unapply(args: List[Tree]): Option[Doc] =
    args match {
      case (arg: Term.PartialFunction) :: Nil =>
        Some(print(arg))
      case (arg @ Term.Function(_, Term.Block(_ :: _ :: _))) :: Nil =>
        Some(dFunction(arg))
      case (Term.Block((f: Term.Function) :: Nil)) :: Nil =>
        Some(dFunction(f))
      case _ =>
        None
    }
}