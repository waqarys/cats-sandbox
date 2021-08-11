package casestudies.typeconstructorsandmonads

import org.scalatest.freespec.AnyFreeSpec

class UptimeClientTest extends AnyFreeSpec {

  "Test total uptime " in {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }
}
