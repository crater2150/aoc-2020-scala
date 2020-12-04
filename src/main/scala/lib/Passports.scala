package aoc2020.lib

object Passports {
  opaque type Passport = Map[String, String]
  object Passport {
      def apply(fields: Map[String, String]): Passport = fields
      def fromStrings(lines: List[String]): Passport =
        (
        for 
          l     <- lines
          field <- l.splitNN(" ", 0)
          pair  <- field.splitOnce(":")
        yield pair
        ).toMap

  }

  extension (p: Passport) {
    def hasRequiredFields: Boolean = requiredFields.forall(p.contains)

    def isValid: Boolean = p.forall {
      case ("byr", year) => (1920 to 2002).contains(year.toInt)
      case ("iyr", year) => (2010 to 2020).contains(year.toInt)
      case ("eyr", year) => (2020 to 2030).contains(year.toInt)
      case ("hgt", height) => height match {
        case s"${cm}cm" => (150 to 193).contains(cm.toInt)
        case s"${inch}in" => (59 to 76).contains(inch.toInt)
        case _ => false
      }
        case ("hcl", hexColor) => colorRegex.matches(hexColor)
        case ("ecl", namedColor) => eyeColors.contains(namedColor)
        case ("pid", passId) => idRegex.matches(passId)
        case ("cid", _) => true
        case (_, _) => false // unknown field
    }
  }

  val requiredFields = Set(
    "byr", //(Birth Year)
    "iyr", //(Issue Year)
    "eyr", //(Expiration Year)
    "hgt", //(Height)
    "hcl", //(Hair Color)
    "ecl", //(Eye Color)
    "pid", //(Passport ID)
    )
  val allFields = requiredFields + "cid"

  val colorRegex = raw"#[0-9a-fA-F]{6}".r
  val eyeColors: Set[String] = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  val idRegex = raw"[0-9]{9}".r
}
