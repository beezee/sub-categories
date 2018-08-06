package bz

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.{Free, Monad, ~>}
import scalaz.Id.Id
import scalaz.std.option._

trait SubCategory[F[_], TF[_[_]]] {
  def interp[G[_]](implicit tf: TF[G]): (F ~> G)
}

object SubCategory {
  def apply[F[_], TF[_[_]]](implicit ta: SubCategory[F, TF]): SubCategory[F, TF] = ta
}

object example extends App {

  trait Show[A]
  case class putStrLn(in: String) extends Show[Unit]

  implicit def showToFree[A](sop: Show[A]): Free[Show, A] = Free.liftF(sop)

  trait ShowAlg[F[_]] {
    def putStrLn(in: String): F[Unit]
  }

  implicit val showAlgId = new ShowAlg[Id] {
    def putStrLn(in: String): Id[Unit] = println(in)
  }

  implicit val showAlgOption = new ShowAlg[Option] {
    def putStrLn(in: String): Option[Unit] = Some(()).map(_ => println(in))
  }

  implicit val taShow: SubCategory[Show, ShowAlg] = new SubCategory[Show, ShowAlg] {
    def interp[G[_]](implicit S: ShowAlg[G]) = new (Show ~> G) {
      def apply[A](sa: Show[A]) = sa match {
        case putStrLn(in) => S.putStrLn(in)
      }
    }
  }

  val prog = for {
    _ <- putStrLn("hi")
    _ <- putStrLn("hello")
    _ <- putStrLn("there")
  } yield "foo"

  prog.foldMap(taShow.interp[Id])
  prog.foldMap(taShow.interp[Option]).foreach(println(_))
}
