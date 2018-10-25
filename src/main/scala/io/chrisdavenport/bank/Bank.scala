package io.chrisdavenport.bank

import io.chrisdavenport.vault._
import cats.implicits._
import cats._
import cats.effect.Sync
import cats.effect.concurrent.Ref

/**
 * Banks Own a Vault but they won't let you take it with you.
 */
trait Bank[F[_]]{
  def createKey[A]: F[Key[A]]
  def lookup[A](k: Key[A]): F[Option[A]]
  def lookupOrError[A](k: Key[A]): F[A]
  def empty : F[Unit]
  def insert[A](k: Key[A], a: A): F[Unit]
  def delete[A](k: Key[A]): F[Unit]
  def adjust[A](k: Key[A], f: A => A): F[Unit]
  def mapK[G[_]](fk: F ~> G): Bank[G] = Bank.mapK(this)(fk)
}
object Bank {
  def apply[F[_]](implicit ev: Bank[F]): Bank[F] = ev

  def build[F[_]: Sync]: F[Bank[F]] = 
    Ref.of[F, Vault](Vault.empty).map(new BankImpl(_))

  def mapK[F[_], G[_]](bank: Bank[F])(fk: F ~> G): Bank[G] = 
    new BankMapKImpl[F, G](bank, fk)

  private class BankImpl[F[_]: Sync](ref: Ref[F, Vault]) extends Bank[F]{
    def createKey[A]: F[Key[A]] = Key.newKey[F, A]
    def lookup[A](k: Key[A]): F[Option[A]] = 
      ref.get.map(_.lookup(k))
    def lookupOrError[A](k: Key[A]): F[A] =
      ref.get
        .map(_.lookup(k))
        .flatMap(_.fold(Sync[F].raiseError[A](new NoSuchElementException))(_.pure[F]))
    def empty : F[Unit] = ref.set(Vault.empty)
    def insert[A](k: Key[A], a: A): F[Unit] = 
      ref.update(_.insert(k, a))
    def delete[A](k: Key[A]): F[Unit] =
      ref.update(_.delete(k))
    def adjust[A](k: Key[A], f: A => A): F[Unit] =
      ref.update(_.adjust(k, f))
  }

  private class BankMapKImpl[F[_], G[_]](bank: Bank[F], fk: F ~> G) extends Bank[G] {
    def createKey[A]: G[Key[A]] = 
      fk(bank.createKey)
    def lookup[A](k: Key[A]): G[Option[A]] = 
      fk(bank.lookup(k))
    def lookupOrError[A](k: Key[A]): G[A] = 
      fk(bank.lookupOrError(k))
    def empty : G[Unit] = 
      fk(bank.empty)
    def insert[A](k: Key[A], a: A): G[Unit] =
      fk(bank.insert(k, a))
    def delete[A](k: Key[A]): G[Unit] = 
      fk(bank.delete(k))
    def adjust[A](k: Key[A], f: A => A): G[Unit] =
      fk(bank.adjust(k, f))
  }
} 