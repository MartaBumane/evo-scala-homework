package errorhandling

object ErrorHandling {
    import cats.data.ValidatedNec
    import cats.syntax.all._

    final case class CardHolderName(name: String) extends AnyVal
    final case class CardNumber(number: Long) extends AnyVal
    final case class CardSecurityCode(securityCode: Int) extends AnyVal
    final case class CardExpirationDate(expirationDate: String) extends AnyVal

    case class PaymentCard(
        name: CardHolderName,
        number: CardNumber,
        securityCode: CardSecurityCode,
        expirationDate: CardExpirationDate,
    )

    sealed trait ValidationError
    object ValidationError {
        final case object CardHolderNameEmpty extends ValidationError {
            override def toString: String = "Cardholders name empty"
        }
        final case object CardHolderNameLength extends ValidationError {
            override def toString: String = "Cardholders name must be at least 7 characters and cannot be longer as 30 characters"
        }
        final case object CardHolderNameContainsSpecialCharacters extends ValidationError {
            override def toString: String = "Cardholders name cannot contain special characters"
        }
        final case object CardNumberLengthError extends ValidationError {
            override def toString: String = "Credit card number must contain 16 characters"
        }
        final case object CardNumberError extends ValidationError {
            override def toString: String = "Card number must contain only digits"
        }
        final case object SecurityCodeCharacterError extends ValidationError {
            override def toString: String = "Security code can contain only digits"
        }
        final case object SecurityCodeLengthError extends ValidationError {
            override def toString: String = "Security code must contain 3 digits!"
        }
        final case object ExpirationDateFormatError extends ValidationError {
            override def toString: String = "Date must contain only digits"
        }
        final case object ExpirationDateExpiredError extends ValidationError {
            override def toString: String = "The Date doesn't exist"
        }
    }

    object PaymentCardValidator {

        type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

        def validateName(name: String): AllErrorsOr[CardHolderName] = {
            def validateNameNotEmpty: AllErrorsOr[CardHolderName] = {
                if (name.length >= 7 && name.length <= 30) {
                    CardHolderName(name).validNec
                } else {
                    CardholderNameEmpty.invalidNec
                }
            }
            def validateNameLengthValid: AllErrorsOr[CardHolderName] = {
                if (name.length > 0) {
                    CardHolderName(name).validNec
                } else {
                    CardHolderNameLength.invalidNec
                }
            }

            def validateNameCharacters: AllErrorsOr[CardHolderName] = {
                if (name.matches("^[a-zA-Z]+$")) {
                    CardHolderName(name).validNec
                } else {
                    CardHolderNameContainsSpecialCharacters.invalidNec
                }
            }

            validateNameNotEmpty andThen validateNameLengthValid andThen validateNameCharacters
        }

        def validateCardNumber(cardNumber: String): AllErrorsOr[CardNumber] = {
            def validateCardNumberLength: AllErrorsOr[CardNumber] = {
                if (cardNumber.length == 16) {
                    CardNumber(cardNumber).validNec
                } else {
                    CardNumberLengthError.invalidNec
                }
            }
            def validateCardNumberCharacters: AllErrorsOr[CardNumber] = {
                if (cardNumber.forall(x=> x.isDigit)) {
                    CardNumber(cardNumber).validNec
                } else {
                    CardNumberError.invalidNec
                }
            }

            validateCardNumberLength andThen validateCardNumberCharacters
        }

        def validateSecurityCode(securityCode: String): AllErrorsOr[CardSecurityCode] = {
            def validateSecurityCodeLength: AllErrorsOr[CardSecurityCode] = {
                if(securityCode.length == 3) CardSecurityCode(securityCode).validNec
                else SecurityCodeLengthError.invalidNec
            }
            def validateSecurityCodeCharacters: AllErrorsOr[CardSecurityCode] = {
                if (securityCode.forall(x=> x.isDigit)) CardSecurityCode(securityCode).validNec
                else  SecurityCodeCharacterError.invalidNec
            }

            validateSecurityCodeLength andThen validateSecurityCodeCharacters
        }

        def validateExpirationDate(expirationDate: String): AllErrorsOr[CardExpirationDate] = {
            def validateExpirationDate: AllErrorsOr[CardExpirationDate] = {
                Try(new SimpleDateFormat("dd.MM.yyyy").parse(expirationDate)).toOption match {
                    case Some(d) => CardExpirationDate(d).validNec
                    case None => Try(new SimpleDateFormat("dd/MM/yyyy").parse(CardExpirationDate(expirationDate))).toOption match {
                        case Some(d) => CardExpirationDate(d).validNec
                        case None => Try(new SimpleDateFormat("yyyyMMdd").parse(CardExpirationDate(expirationDate))).toOption match {
                            case Some(d) => CardExpirationDate(d).validNec
                            case None => ExpirationDateFormatError.invalidNec
                        }
                    }
                }
            }

            def validateExpirationDateNotExpired(expirationDate: Date): AllErrorsOr[CardExpirationDate] = {
                if (expirationDate.isAfter(CardExpirationDate.now())){
                    CardExpirationDate(expirationDate).validNec
                } else {
                    ExpirationDateExpiredError.invalidNec
                }
            }

            validateExpirationDate andThen validateExpirationDateNotExpired
        }

        def validate(
            name: String,
            number: String,
            expirationDate: String,
            securityCode: String,
        ): AllErrorsOr[PaymentCard] = (validateName(name), validateCardNumber(number), validateSecurityCode(securityCode), validateExpirationDate(expirationDate)).mapN(PaymentCard)
    }
}
