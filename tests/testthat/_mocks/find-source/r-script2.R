# This script mocks a real R script containing source code.

translate("a")
transltr::translate("b")
embeddedCall(translate("c"))
embeddedCall(transltr::translate("d"))
tr$translate("e")
tr$translate("f")
