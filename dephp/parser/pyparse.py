import pyparsing as pp

start_php = pp.Word("<?") | pp.Word("<?php")
end_php = pp.Word("?>") | pp.stringEnd

php_code = pp.SkipTo(end_php)
stuff = pp.SkipTo(pp.stringEnd).leaveWhitespace()

php_program = pp.stringEnd | stuff
