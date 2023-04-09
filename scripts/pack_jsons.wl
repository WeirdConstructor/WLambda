#!wlambda
!packed =
    std:bytes:lzw:encode
        (std:str:to_bytes
            (std:ser:json
                (std:deser:json
                    (std:io:file:read_text "src/chemical_elements.json"))
                    $true))
        8;
std:io:file:write_safe "src/chemical_elements.json.lzw" packed;
