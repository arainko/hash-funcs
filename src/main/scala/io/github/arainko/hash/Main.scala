package io.github.arainko.hash

import org.kocakosm.jblake2.Blake2b
import scodec.bits._
import zio._
import zio.stream._

import java.nio.file.Paths

object Main extends App {

  private val algorithms = "MD5" :: "SHA-1" :: "SHA-224" :: "SHA-256" :: "SHA-384" :: "SHA-512" :: "B2" :: Nil

  private val hashPdf = ZManaged
    .readFile("files/hash.pdf")
    .mapM(_.readAll(4096))
    .map(ByteVector.apply)

  private val personal = ZManaged
    .readFile("files/personal.txt")
    .mapM(_.readAll(4096))
    .map(ByteVector.apply)

  private val personalLn = ZManaged
    .readFile("files/personal_.txt")
    .mapM(_.readAll(4096))
    .map(ByteVector.apply)

  private val files =
    for {
      hashPdf    <- hashPdf
      personal   <- personal
      personalLn <- personalLn
      concatPersonal   = hashPdf ++ personal
      concatPersonalLn = hashPdf ++ personalLn
    } yield (concatPersonal, concatPersonalLn)

  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    files
      .map { case (concatPersonal, concatPersonalLn) =>
        algorithms.map { alg =>
          val concatHash = if (alg == "B2") ByteVector {
            new Blake2b(64).digest(concatPersonal.toArray: _*)
          } else concatPersonal.digest(alg)

          val concatLnHash = if (alg == "B2") ByteVector {
            new Blake2b(64).digest(concatPersonalLn.toArray: _*)
          } else concatPersonalLn.digest(alg)

          val bitCount = concatLnHash.bits.size

          val diff = List.unfold(bitCount - 1) { index =>
            Option.when(index != -1) {
              (concatLnHash.bits(index) == concatHash.bits(index)) -> (index - 1)
            }
          }

          val bitDiff        = diff.filter(_ == true).size
          val bitDiffPercent = (bitDiff.toDouble / bitCount * 100).round

          s"""
             |Algorithm: ${alg}
             |hash.pdf ++ personal.txt
             |hash.pdf ++ personal_.txt
             |${concatHash.toHex}
             |${concatLnHash.toHex}
             |Bit diff: $bitDiff / $bitCount ($bitDiffPercent%)
      """.stripMargin
        }
      }
      .use { results =>
        ZStream
          .fromIterable(results)
          .map(_.getBytes)
          .map(Chunk.fromArray)
          .flattenChunks
          .run(ZSink.fromFile(Paths.get("files/diff.txt")))
      }
      .exitCode

}
