

package com.github.hiroshinke.cobolsample;

import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.RuleContext;
import org.antlr.v4.runtime.DefaultErrorStrategy;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.InputMismatchException;

public class CobolErrorStrategy extends DefaultErrorStrategy {


    public void sync(Parser recognizer)
	throws RecognitionException
    {
	/*
	RuleContext rc = recognizer.getContext();

	int index = recognizer.getContext().getRuleIndex();
	System.err.println("Sync: " + recognizer.getRuleNames()[index] );
	*/

	int ttype = recognizer.getInputStream().LA(1);
	if( ttype == Cobol85Parser.COPY) {
	    Token t = recognizer.consume();
	    System.err.println("skip token: " + t.toString());
	    while( true ){
		ttype = recognizer.getInputStream().LA(1);
		if( ttype == Cobol85Parser.DOT_FS) {
		    t = recognizer.consume();
		    System.err.println("skip token: " + t.toString());
		    break;
		}
		t = recognizer.consume();
		System.err.println("skip token: " + t.toString());
	    }
	    
	} else {
	    super.sync(recognizer);
	}
    }

    /*
    @Override
    public void recover(Parser recognizer, RecognitionException e) {
	if (e instanceof InputMismatchException) {
	    int ttype = recognizer.getInputStream().LA(1);
	    while (ttype != Token.EOF && ttype != Cobol85Parser.DOT_FS) {
		Token t = recognizer.consume();
		System.err.println("cosume token: " + t.toString());
		ttype = recognizer.getInputStream().LA(1);
	    }
	    if( ttype == Cobol85Parser.DOT_FS) {
		Token t = recognizer.consume();
		System.err.println("cosume token: " + t.toString());
	    }
 	} else {
	    super.recover(recognizer, e);
	}
    }
    */
}
