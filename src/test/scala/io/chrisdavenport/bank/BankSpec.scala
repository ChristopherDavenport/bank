package io.chrisdavenport.bank

import org.specs2._
import cats.effect._


object BankSpec extends mutable.Specification with ScalaCheck {

  "Bank" should {
    "be able to insert a key"  >> prop { i: Int => 
      val test = for {
        bank <- Bank.build[IO]
        intKey <- bank.createKey[Int]
        _ <- bank.insert(intKey, i)
        out <- bank.lookupOrError(intKey)
      } yield out

      test.unsafeRunSync must_=== i
    }
  }

}