=== MLSTRUCT SCALA REFERENCE IMPLEMENTATION FINDINGS ===

Key Files and Line Numbers:

1. NOTHING/BOTTOM TYPE DEFINITION:
    - /Users/nico/dev/hkust-taco/mlstruct/shared/src/main/scala/mlscript/TyperDatatypes.scala
      Lines 144-148: ExtrType definition with polarity
        - polarity=true means Bottom/Nothing (⊥)
        - polarity=false means Top/Any (⊤)

2. POLARITY TRACKING & TYPE VARIABLES:
    - /Users/nico/dev/hkust-taco/mlstruct/shared/src/main/scala/mlscript/TyperDatatypes.scala
      Lines 313-320: isRecursive_$ and polarity tracking via getVarsPol
        - Tracks type variables at positive/negative/invariant (neutral) polarity
        - Used to determine which bounds are relevant for simplification

3. POLARITY-AWARE TYPE SIMPLIFICATION:
    - /Users/nico/dev/hkust-taco/mlstruct/shared/src/main/scala/mlscript/TypeSimplifier.scala
      Lines 15-86: removeIrrelevantBounds function
        - Key implementation of bounds simplification respecting polarity
        - Line 19: pols = ty.getVarsPol(S(true)) - gets all type variable polarities
        - Lines 41-46: Special handling:
            * Line 42: Only process lower bounds if forall polarity === positive AND bounds exist
              renewed.lowerBounds = if (pols(tv).forall(_ === true))
              tv.lowerBounds.iterator.map(process(_, S(true -> tv))).reduceOption(_ | _).filterNot(_.isBot).toList
              else Nil
            * Line 45: Only process upper bounds if forall polarity === false AND bounds exist
              renewed.upperBounds = if (pols(tv).forall(_ === false))
              tv.upperBounds.iterator.map(process(_, S(false -> tv))).reduceOption(_ & _).filterNot(_.isTop).toList
              else Nil

4. NORMAL FORMS & BOTTOM TYPE:
    - /Users/nico/dev/hkust-taco/mlstruct/shared/src/main/scala/mlscript/NormalForms.scala
      Lines 257-278: RhsNf trait with bottom type handling
        - Line 258: def isBot: Bool = isInstanceOf[RhsBot.type]
        - Line 277: case object RhsBot with toString = "⊥"
        - Line 157: mkType handles RhsBot -> BotType
        - Line 160: RhsBot case directly returns BotType

5. POLARITY IN VARIANCE ANALYSIS:
    - /Users/nico/dev/hkust-taco/mlstruct/shared/src/main/scala/mlscript/TypeDefs.scala
      Lines 568-713: computeVariances function
        - Lines 583-585: Comments explain polarity concept:
          "true polarity if covariant position visit"
          "false polarity if contravariant position visit"
        - Lines 608-615: Variance decision logic based on polarity
            * Line 609-610: Checks if polarity is contravariant/covariant
            * Line 614: Process lower bounds at positive polarity
            * Line 615: Process upper bounds at negative polarity

6. CHILDREN WITH POLARITY TRACKING:
    - /Users/nico/dev/hkust-taco/mlstruct/shared/src/main/scala/mlscript/TyperHelpers.scala
      Lines 390-433: childrenPol method (implicit in type class)
        - Line 415-432: childrenPol implementation details:
            * Line 417: Function arguments are contravariant (negative polarity)
            * Line 418: Function return types are covariant (same polarity)
            * Line 423: Union types preserve polarity
            * Line 425: Intersection types preserve polarity
            * Line 428: Negation inverts polarity
            * Line 432: TypeRef applies per-parameter variance

7. GETVARSPOL IMPLEMENTATION:
    - /Users/nico/dev/hkust-taco/mlstruct/shared/src/main/scala/mlscript/TyperHelpers.scala
      Lines 435-461: getVarsPol function
        - Recursive traversal collecting type variable polarities
        - Line 441: Pattern for (tvp, tv: TypeVariable)
            * tvp is the polarity context at that location
        - Lines 442-453: Logic for handling polarities:
            * Line 443: If already seen with no polarity (N), skip
            * Line 444: If same polarity, skip
            * Line 445-449: If different non-neutral polarities seen before:
              res += tv -> N (invariant/mixed polarity)
            * Line 450-453: First time seeing this variable, record its polarity
            * Line 449, 453: Recursively process childrenPol with the current polarity

CRITICAL INSIGHT - KEY DIFFERENCE FROM ILE:
The MLStruct reference checks `forall(_ === polarity)` meaning ALL uses of a type variable
at the same location must have consistent polarity. This differs from the Ile implementation
which may assign a single polarity per variable globally.

See TypeSimplifier.scala line 31-32:
"MLStruct's getVarsPol checks forall(_ === polarity) - TypeSimplifier.scala:19
but Ile's getVarsPolFor assigns single polarity per variable. This may miss mixed-polarity cases."
