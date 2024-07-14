encode_subject <- function(subject) {
    sprintf("=?UTF-8?B?%s?=", base64enc::base64encode(charToRaw(enc2utf8(subject))))
}
