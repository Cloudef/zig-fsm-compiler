/*
 * Copyright 2006-2007 Adrian Thurston <thurston@colm.net>
 * Copyright 2011 Josef Goettgens
 * Copyright 2023 Vetoniemi Jari
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "rlscan.h"

enum InlineBlockType
{
	CurlyDelimited,
	SemiTerminated
};

void error(struct Scanner *scanner, const char *msg) {
	scanner->error(scanner, msg);
}

enum TokenType token_from_char(struct Scanner *scanner, char c) {
	switch (c) {
		case '.': return TK_Dot;
		case '=': return TK_Equals;
		case ';': return TK_SemiColon;
		case '(': return TK_ParenOpen;
		case ')': return TK_ParenClose;
		case ',': return TK_Comma;
		case '+': return RE_Repeat;
		case '*': return RE_Star;
		case '?': return RE_Maybe;
		case '@': return TK_At;
		case '$': return TK_Dollar;
		case '%': return TK_Percent;
		case '>': return TK_Gt;
		case '|': return TK_Union;
		case '!': return TK_Negation;
	};
	fprintf(stderr, "unknown `%c`\n", c);
	error(scanner, "unknown token");
	return unknown;
}

void token3(struct Scanner *scanner, enum TokenType type, char *start, char *end, bool is_char) {
	if (end != NULL) {
		if (is_char) {
			scanner->token(scanner, type, (void*)start[0], true);
		} else {
			char old = *end;
			*end = 0;
			scanner->token(scanner, type, start, false);
			*end = old;
		}
	} else {
		scanner->token(scanner, type, NULL, false);
	}
}

void token2(struct Scanner *scanner, enum TokenType type, char c) {
	scanner->token(scanner, type, (void*)c, true);
}

void token1(struct Scanner *scanner, enum TokenType type) {
	token3(scanner, type, 0, 0, false);
}

%%{
	machine rlscan;

	# This is sent by the driver code.
	EOF = 0;

	action inc_nl {
		lastnl = p;
		column = 0;
		line++;
	}
	NL = '\n' @inc_nl;

	# Identifiers, numbers, commetns, and other common things.
	ident = ( alpha | '_' ) ( alpha |digit |'_' )*;
	ocaml_ident = ( alpha | '_' ) ( alpha |digit |'_' )* "'"?;
	number = digit+;
	hex_number = '0x' [0-9a-fA-F]+;

	c_comment =
		'/*' ( any | NL )* :>> '*/';

	cpp_comment =
		'//' [^\n]* NL;

	c_cpp_comment = c_comment | cpp_comment;

	# These literal forms are common to host code and ragel.
	s_literal = "'" ([^'\\] | NL | '\\' (any | NL))* "'";
	d_literal = '"' ([^"\\] | NL | '\\' (any | NL))* '"';
	host_re_literal = '/' ([^/\\] | NL | '\\' (any | NL))* '/';

	whitespace = [ \t] | NL;
	pound_comment = '#' [^\n]* NL;

	# An inline block of code for languages other than Ruby.
	inline_code := |*
		# Inline expression keywords.
		"fpc" => { token1( scanner, KW_PChar ); };
		"fc" => { token1( scanner, KW_Char ); };
		"fcurs" => { token1( scanner, KW_CurState ); };
		"ftargs" => { token1( scanner, KW_TargState ); };
		"fentry" => {
			token1( scanner, KW_Entry );
		};

		# Inline statement keywords.
		"fhold" => {
			token1( scanner, KW_Hold );
		};
		"fexec" => { token1( scanner, KW_Exec ); };
		"fgoto" => {
			token1( scanner, KW_Goto );
		};
		"fnext" => {
			token1( scanner, KW_Next );
		};
		"fcall" => {
			token1( scanner, KW_Call );
		};
		"fret" => {
			token1( scanner, KW_Ret );
		};
		"fbreak" => {
			token1( scanner, KW_Break );
		};
		"fncall" => {
			token1( scanner, KW_Ncall );
		};
		"fnret" => {
			token1( scanner, KW_Nret );
		};
		"fnbreak" => {
			token1( scanner, KW_Nbreak );
		};

		ident => { token3( scanner, TK_Word, ts, te, false ); };

		number => { token3( scanner, TK_UInt, ts, te, false ); };
		hex_number => { token3( scanner, TK_Hex, ts, te, false ); };

		( s_literal | d_literal )
			=> { token3( scanner, IL_Literal, ts, te, false ); };

		whitespace+ => {};

		c_cpp_comment => { token3( scanner, IL_Comment, ts, te, false ); };

		"::" => { token3( scanner, TK_NameSep, ts, te, false ); };

		# Some symbols need to go to the parser as with their cardinal value as
		# the token type (as opposed to being sent as anonymous symbols)
		# because they are part of the sequences which we interpret. The * ) ;
		# symbols cause whitespace parsing to come back on. This gets turned
		# off by some keywords.

		";" => {
			token1( scanner, TK_SemiColon );
			if ( inlineBlockType == SemiTerminated )
				fret;
		};

		"$" [a-zA-Z_][a-zA-Z_0-9]* => {
			token3( scanner, IL_Symbol, ts, ts+1, false );
			fexec ts+1;
		};

		[*)] => {
			token3( scanner, token_from_char(scanner, *ts), ts, te, false );
		};

		[,(] => { token3( scanner, token_from_char(scanner, *ts), ts, te, false ); };

		'{' => {
			token3( scanner, IL_Symbol, ts, te, false );
			curly_count += 1;
		};

		'}' => {
			if ( --curly_count == 0 && inlineBlockType == CurlyDelimited ) {
				/* Inline code block ends. */
				token1( scanner, TK_CurlyClose );
				fret;
			}
			else {
				/* Either a semi terminated inline block or only the closing
				 * brace of some inner scope, not the block's closing brace. */
				token3( scanner, IL_Symbol, ts, te, false );
			}
		};

		EOF => {
			error( scanner, "unterminated code block");
		};

		# Send every other character as a symbol.
		any => { token3( scanner, IL_Symbol, ts, te, false ); };
	*|;

	or_literal := |*
		# Escape sequences in OR expressions.
		'\\0' => { token2( scanner, RE_Char, '\0' ); };
		'\\a' => { token2( scanner, RE_Char, '\a' ); };
		'\\b' => { token2( scanner, RE_Char, '\b' ); };
		'\\t' => { token2( scanner, RE_Char, '\t' ); };
		'\\n' => { token2( scanner, RE_Char, '\n' ); };
		'\\v' => { token2( scanner, RE_Char, '\v' ); };
		'\\f' => { token2( scanner, RE_Char, '\f' ); };
		'\\r' => { token2( scanner, RE_Char, '\r' ); };
		'\\\n' => {};
		'\\' any => { token3( scanner, RE_Char, ts+1, te, true ); };

		# Range dash in an OR expression.
		'-' => { token1( scanner, RE_Dash ); };

		# Terminate an OR expression.
		']'	=> { token1( scanner, RE_SqClose ); fret; };

		EOF => {
			error( scanner, "unterminated OR literal");
		};

		# Characters in an OR expression.
		[^\]] => { token3( scanner, RE_Char, ts, te, true ); };

	*|;

	ragel_re_literal := |*
		# Escape sequences in regular expressions.
		'\\0' => { token2( scanner, RE_Char, '\0' ); };
		'\\a' => { token2( scanner, RE_Char, '\a' ); };
		'\\b' => { token2( scanner, RE_Char, '\b' ); };
		'\\t' => { token2( scanner, RE_Char, '\t' ); };
		'\\n' => { token2( scanner, RE_Char, '\n' ); };
		'\\v' => { token2( scanner, RE_Char, '\v' ); };
		'\\f' => { token2( scanner, RE_Char, '\f' ); };
		'\\r' => { token2( scanner, RE_Char, '\r' ); };
		'\\\n' => {};
		'\\' any => { token3( scanner, RE_Char, ts+1, te, true ); };

		# Terminate an OR expression.
		'/' [i]? => {
			token3( scanner, RE_Slash, ts, te, false );
			fgoto parser_def;
		};

		# Special characters.
		'.' => { token1( scanner, RE_Dot ); };
		'*' => { token1( scanner, RE_Star ); };

		'[' => { token1( scanner, RE_SqOpen ); fcall or_literal; };
		'[^' => { token1( scanner, RE_SqOpenNeg ); fcall or_literal; };

		EOF => {
			error( scanner, "unterminated regular exrpession");
		};

		# Characters in an OR expression.
		[^\/] => { token3( scanner, RE_Char, ts, te, true ); };
	*|;

	# We need a separate token space here to avoid the ragel keywords.
	write_statement := |*
		ident => { token3( scanner, TK_Word, ts, te, false ); } ;
		[ \t\n]+ => {};
		';' => { token1( scanner, TK_SemiColon ); fgoto parser_def; };

		EOF => {
			error( scanner, "unterminated write statement");
		};
	*|;

	# Parser definitions.
	parser_def := |*
		#'length_cond' => { token1( scanner, KW_Length ); };
		'machine' => { token1( scanner, KW_Machine ); };
		'include' => { token1( scanner, KW_Include ); };
		'import' => { token1( scanner, KW_Import ); };
		'write' => {
			token1( scanner, KW_Write );
			fgoto write_statement;
		};
		'action' => { token1( scanner, KW_Action ); };
		'alphtype' => { token1( scanner, KW_AlphType ); };
		'prepush' => { token1( scanner, KW_PrePush ); };
		'postpop' => { token1( scanner, KW_PostPop ); };

		'nfaprepush' => { token1( scanner, KW_NfaPrePush ); };
		'nfapostpop' => { token1( scanner, KW_NfaPostPop ); };

		# FIXME: Enable this post 5.17.
		# 'range' => { token1( scanner, KW_Range ); };

		'getkey' => {
			token1( scanner, KW_GetKey );
			inlineBlockType = SemiTerminated;
			fcall inline_code;
		};
		'access' => {
			token1( scanner, KW_Access );
			inlineBlockType = SemiTerminated;
			fcall inline_code;
		};
		'variable' => {
			token1( scanner, KW_Variable );
			inlineBlockType = SemiTerminated;
			fcall inline_code;
		};
		'when' => { token1( scanner, KW_When ); };
		'inwhen' => { token1( scanner, KW_InWhen ); };
		'outwhen' => { token1( scanner, KW_OutWhen ); };
		'eof' => { token1( scanner, KW_Eof ); };
		'err' => { token1( scanner, KW_Err ); };
		'lerr' => { token1( scanner, KW_Lerr ); };
		'to' => { token1( scanner, KW_To ); };
		'from' => { token1( scanner, KW_From ); };
		'export' => { token1( scanner, KW_Export ); };

		# Identifiers.
		ident => { token3( scanner, TK_Word, ts, te, false ); } ;

		# Numbers
		number => { token3( scanner, TK_UInt, ts, te, false ); };
		hex_number => { token3( scanner, TK_Hex, ts, te, false ); };

		# Literals, with optionals.
		( s_literal | d_literal ) [i]?
			=> { token3( scanner, TK_Literal, ts, te, false ); };

		'[' => { token1( scanner, RE_SqOpen ); fcall or_literal; };
		'[^' => { token1( scanner, RE_SqOpenNeg ); fcall or_literal; };

		'/' => { token1( scanner, RE_Slash ); fgoto ragel_re_literal; };

		# Ignore.
		pound_comment => {};

		':=' => { token1( scanner, TK_ColonEquals ); };
		'|=' => { token1( scanner, TK_BarEquals ); };

		# To State Actions.
		">~" => { token1( scanner, TK_StartToState ); };
		"$~" => { token1( scanner, TK_AllToState ); };
		"%~" => { token1( scanner, TK_FinalToState ); };
		"<~" => { token1( scanner, TK_NotStartToState ); };
		"@~" => { token1( scanner, TK_NotFinalToState ); };
		"<>~" => { token1( scanner, TK_MiddleToState ); };

		# From State actions
		">*" => { token1( scanner, TK_StartFromState ); };
		"$*" => { token1( scanner, TK_AllFromState ); };
		"%*" => { token1( scanner, TK_FinalFromState ); };
		"<*" => { token1( scanner, TK_NotStartFromState ); };
		"@*" => { token1( scanner, TK_NotFinalFromState ); };
		"<>*" => { token1( scanner, TK_MiddleFromState ); };

		# EOF Actions.
		">/" => { token1( scanner, TK_StartEOF ); };
		"$/" => { token1( scanner, TK_AllEOF ); };
		"%/" => { token1( scanner, TK_FinalEOF ); };
		"</" => { token1( scanner, TK_NotStartEOF ); };
		"@/" => { token1( scanner, TK_NotFinalEOF ); };
		"<>/" => { token1( scanner, TK_MiddleEOF ); };

		# Global Error actions.
		">!" => { token1( scanner, TK_StartGblError ); };
		"$!" => { token1( scanner, TK_AllGblError ); };
		"%!" => { token1( scanner, TK_FinalGblError ); };
		"<!" => { token1( scanner, TK_NotStartGblError ); };
		"@!" => { token1( scanner, TK_NotFinalGblError ); };
		"<>!" => { token1( scanner, TK_MiddleGblError ); };

		# Local error actions.
		">^" => { token1( scanner, TK_StartLocalError ); };
		"$^" => { token1( scanner, TK_AllLocalError ); };
		"%^" => { token1( scanner, TK_FinalLocalError ); };
		"<^" => { token1( scanner, TK_NotStartLocalError ); };
		"@^" => { token1( scanner, TK_NotFinalLocalError ); };
		"<>^" => { token1( scanner, TK_MiddleLocalError ); };

		# Middle.
		"<>" => { token1( scanner, TK_Middle ); };

		# Conditions.
		'>?' => { token1( scanner, TK_StartCond ); };
		'$?' => { token1( scanner, TK_AllCond ); };
		'%?' => { token1( scanner, TK_LeavingCond ); };

		'..'   => { token1( scanner, TK_DotDot ); };
		'../i' => { token1( scanner, TK_DotDotIndep ); };

		'**' => { token1( scanner, TK_StarStar ); };
		'--' => { token1( scanner, TK_DashDash ); };
		'->' => { token1( scanner, TK_Arrow ); };
		'=>' => { token1( scanner, TK_DoubleArrow ); };

		":>"  => { token1( scanner, TK_ColonGt ); };
		":>>" => { token1( scanner, TK_ColonGtGt ); };
		"<:"  => { token1( scanner, TK_LtColon ); };

		":nfa("   => { token1( scanner, TK_ColonNfaOpen ); };
		":cond("  => { token1( scanner, TK_ColonCondOpen ); };
		":condstar("  => { token1( scanner, TK_ColonCondStarOpen ); };
		":condplus("  => { token1( scanner, TK_ColonCondPlusOpen ); };
		":nomax(" => { token1( scanner, TK_ColonNoMaxOpen ); };
		"):"      => { token1( scanner, TK_CloseColon ); };

		# Opening of longest match.
		"|*" => { token1( scanner, TK_BarStar ); };
		# Closing of longest match.
		"*|" => { token1( scanner, TK_StarBar ); };

		# Separater for name references.
		"::" => { token3( scanner, TK_NameSep, ts, te, false ); };

		'}%%' => {
			fret;
		};

		[ \t\r]+ => {};

		# If we are in a single line machine then newline may end the spec.
		NL => {
			if ( singleLineSpec ) {
				fret;
			}
		};

		'{' => {
			if ( lastToken == KW_Export || lastToken == KW_Entry )
				token1( scanner, TK_CurlyOpen );
			else {
				token1( scanner, TK_CurlyOpen );
				curly_count = 1;
				inlineBlockType = CurlyDelimited;
				fcall inline_code;
			}
		};

		EOF => {
			error( scanner, "unterminated ragel section");
		};

		any => { token1( scanner, token_from_char(scanner, *ts) ); } ;
	*|;

	# Outside code scanner. These tokens get passed through.
	main := |*
		'%%{' => {
			singleLineSpec = false;
			fcall parser_def;
		};
		'%%' => {
			singleLineSpec = true;
			fcall parser_def;
		};
		whitespace+ => {};
		EOF;
		any => {};
	*|;
}%%

%% write data;

void scan(struct Scanner *scanner) {
	int cs, act, have = 0;
	int top;

	/* The stack is two deep, one level for going into ragel defs from the main
	 * machines which process outside code, and another for going into or literals
	 * from either a ragel spec, or a regular expression. */
	int stack[2];
	int curly_count = 0;
	bool singleLineSpec = false;
	enum InlineBlockType inlineBlockType = CurlyDelimited;

	int line = 1;
	int column = 1;
	char *lastnl = 0;
	char *ts = NULL, *te = NULL;
	enum TokenType lastToken = unknown;

	%% write init;

	/* Set up the start state. FIXME: After 5.20 is released the nocs write
	 * init option should be used, the main machine eliminated and this statement moved
	 * above the write init. */
	cs = rlscan_en_main;

	char *p = (char*)scanner->input;
	int len = scanner->input_len;
	char *pe = p + len;
	char *eof = pe;

	%% write exec;

	/* Check if we failed. */
	if (cs == rlscan_error) {
		/* Machine failed before finding a token. I'm not yet sure if this
		 * is reachable. */
		// id->error( scanner, scan_loc()) << "scanner error" << endl;
		// id->abortCompile(1);
		error( scanner, "scanner error");
	}
}
