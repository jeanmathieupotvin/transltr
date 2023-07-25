# $initialize() throws an error if concat is invalid

    `concat` must be a single, non-empty, and non-NA character of any size. See `?nchar()`.

# $initialize() throws an error if it cannot convert values

    all values must be coercible to a character vector.

# $format() throws an error if signatureLength is invalid

    `signatureLength` must be a single integer value greater than or equal to 0L (no `$signature`) and lesser than or equal to 64L (full `$signature`).

# $print() prints signature and string as intended

    [ NOTE ] Using full $signature (by default):
    
    <TranslatableString: c831e66d85ddb21722130ee9b02aedd62ecca0eba2661acb9320057aa4aa568f>
     token1 token2

# $print() applies signatureLength as intended

    [ NOTE ] Using 16 first characters of $signature:
    
    <TranslatableString: c831e66d85ddb217>
     token1 token2

---

    [ NOTE ] Not using $signature:
    
    <TranslatableString>
     token1 token2

# $cat() wraps $string if it is longer 60 characters

    [ NOTE ] String below has 75 characters.
    [ NOTE ] Each line should be indented by 1 space.
    
     Lorem ipsum dolor sit amet, consectetur adipiscing elit.
     Nunc ac urna eros.

---

    [ NOTE ] String below has 463 characters.
    [ NOTE ] Each line should be indented by 1 space.
    
     Lorem ipsum dolor sit amet, consectetur adipiscing elit.
     Nunc ac urna eros. Sed placerat risus et mauris semper, vel
     pulvinar lectus laoreet. Nullam interdum bibendum nunc, at
     iaculis nisi aliquam vel. Suspendisse potenti. Donec orci
     arcu, aliquam non sapien eget, bibendum malesuada arcu. Sed
     ut fermentum mi. Orci varius natoque penatibus et magnis
     dis parturient montes, nascetur ridiculus mus. Donec nulla
     justo, consectetur sed quam ac, lacinia commodo odio.

