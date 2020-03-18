package scim.spi

import scim.model.SortOrder

case class Sorting(byField: String, order: SortOrder)
