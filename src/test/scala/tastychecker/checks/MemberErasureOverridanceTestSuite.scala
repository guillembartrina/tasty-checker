package tastychecker

import tastyquery.*
import tastyquery.Contexts.*
import tastyquery.Exceptions.*
import tastyquery.Spans.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Constants.*
import tastyquery.Trees.*
import tastyquery.Types.*


//Unable to create negative tests because the check takes place in the symbol world and
//it is seems impossible to create artificial symbol+tree+type topologies,
//Dummy testlib contains positive tests.
class MemberErasureOverridanceTestSuite extends BaseCheckTestSuite
