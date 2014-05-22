package org.oopsc.symbol

import org.oopsc.Identifier

/** Represents a reference to a type to be resolved during the reference phase. */
class ResolvableType(var identifier: Identifier, var declaration: Option[Type] = None) {

}