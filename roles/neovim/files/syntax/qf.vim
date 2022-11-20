if exists('b:current_syntax')
	finish
endif

setlocal conceallevel=2
setlocal concealcursor=nvic

syn match qfNonValid	'^}' conceal

syn region qfValid		start='^{' end='$' contains=qfPath,qfPosition,qfError,qfWarn,qfInfo,qfHint

syn match qfValidToken	'^{' contained conceal
syn match qfPath		'^{[^:]\+' nextgroup=qfPosition contains=qfValidToken contained
syn match qfPosition	':[0-9]\+\(:[0-9]\+\)\?\s\+' nextgroup=qfError,qfWarn,qfInfo,qfHint contained

syn match qfError		'E .*$' contained
syn match qfError		'error' contained
syn match qfWarn		'W .*$' contained
syn match qfInfo		'I .*$' contained
syn match qfHint		'[NH] .*$' contained

hi def link qfPath		Directory
hi def link qfPosition	Number

hi def link qfError		DiagnosticError
hi def link qfWarn		DiagnosticWarn
hi def link qfInfo		DiagnosticInfo
hi def link qfHint		DiagnosticHint

let b:current_syntax = 'qf'
