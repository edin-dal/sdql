package sdql
package storage

import ir.Attribute

case class Table(name: String, attributes: Seq[Attribute], resourceLocator: String)