package sdql
package storage

import sdql.ir.Attribute

case class Table(name: String, attributes: Seq[Attribute], resourceLocator: String)
