!chems = std:deser:json ~ std:io:file:read_text "chemical_elements.json";
!nums = std:io:file:read_text "data.txt";

iter line ($p("\n", 0) nums) {
    !fields = $p("\t", 0) line;
    !num = (int ~ std:str:trim fields.0) - 1;
#    chems.(num).group = int fields.3;
#    chems.(num).period = int fields.4;
    !spec_heat = float ~ std:str:trim fields.11;
    !origin = std:str:trim fields.14;
#    std:displayln spec_heat origin;
    chems.(num).origin = origin;
    chems.(num).specific_heat_capacity = spec_heat;
};

std:displayln ~ std:ser:json chems;


