package net.surguy.gpxparser

import java.io.InputStream
import java.time.Instant

import scala.language.postfixOps
import scala.xml.{Elem, XML}

/**
 * Parse a cut-down version of the GPX file format, as produced by Runkeeper.
 *
 * @author Inigo Surguy
 */
class GpxParser {
  def parse(xml: String) = parseGpx(XML.loadString(xml))
  def parse(xmlStream: InputStream) = parseGpx(XML.load(xmlStream))
  def parseGpx(gpx: Elem): Gpx = Gpx(parseGpxMetadata(gpx), (gpx \ "trk").collect{ case e: Elem => parseTrack(e) } )

  private[gpxparser] def parseGpxMetadata(gpx: Elem): GpxMetadata = GpxMetadata(gpx \ "@version" text, gpx \ "@creator" text)
  private[gpxparser] def parseTrack(trk: Elem): Track = {
    Track(trk \ "name" text, Instant.parse(trk \ "time" text),
      (trk \ "trkseg" \ "trkpt").collect{ case e: Elem => parsePoint(e) })
  }

  // <trkpt lat="51.752529000" lon="-1.281438000"><ele>58.0</ele><time>2015-07-26T10:23:47Z</time></trkpt>
  private[gpxparser] def parsePoint(trkpt: Elem): TrackPoint = {
    TrackPoint(Coordinate((trkpt \ "@lat" text).toDouble, (trkpt \ "@lon" text).toDouble),
                (trkpt \ "ele" text).toDouble, Instant.parse(trkpt \ "time" text))
  }
}

case class Gpx(metadata: GpxMetadata, tracks: Seq[Track])
case class GpxMetadata(version: String, creator: String)
case class Track(name: String, startTime: Instant, points: Seq[TrackPoint])
case class TrackPoint(location: Coordinate, elevationInMeters: Double, time: Instant)
case class Coordinate(lat: Double, long: Double)