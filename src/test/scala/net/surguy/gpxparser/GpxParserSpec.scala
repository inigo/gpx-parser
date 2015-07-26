package net.surguy.gpxparser

import java.time.Instant

import org.specs2.mutable.Specification


class GpxParserSpec extends Specification {
  val parser = new GpxParser()

  "Reading components of a GPX file" should {
    "parse a trkpt" in {
      parser.parsePoint(<trkpt lat="50.987654321" lon="-1.123456789"><ele>58.0</ele><time>2015-07-26T10:23:47Z</time></trkpt>) mustEqual
        TrackPoint(Coordinate(50.987654321, -1.123456789), 58D, Instant.parse("2015-07-26T10:23:47Z"))
    }
    "parse metadata" in {
      parser.parseGpxMetadata(<gpx version="1.1" creator="Runkeeper - http://www.runkeeper.com"/>) mustEqual
        GpxMetadata(version = "1.1", creator = "Runkeeper - http://www.runkeeper.com")
    }
    "parse tracks" in {
      val track = parser.parseTrack(
        <trk>
          <name><![CDATA[Running 7/26/15 10:23 am]]></name>
          <time>2015-07-26T10:23:47Z</time>
          <trkseg>
            <trkpt lat="50.987654321" lon="-1.123456789"><ele>58.0</ele><time>2015-07-26T10:23:47Z</time></trkpt>
            <trkpt lat="50.987654325" lon="-1.123456780"><ele>58.0</ele><time>2015-07-26T10:23:48Z</time></trkpt>
          </trkseg>
        </trk>)
      track.name mustEqual "Running 7/26/15 10:23 am"
      track.startTime mustEqual Instant.parse("2015-07-26T10:23:47Z")
      track.points.map(_.time) mustEqual Seq(Instant.parse("2015-07-26T10:23:47Z"), Instant.parse("2015-07-26T10:23:48Z"))
    }
  }

  "Reading a complete GPX file" should {
    "return a valid Gpx object" in {
      val gpx = parser.parse(this.getClass.getResourceAsStream("/runkeeper.gpx"))
      gpx.metadata.creator mustEqual "Runkeeper - http://www.runkeeper.com"
      gpx.tracks.map(_.name) mustEqual Seq("Running 7/26/15 10:23 am")
      gpx.tracks.head.points must haveSize(5648)
    }
  }

}
