import Visitor  from './Visitor.js';

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        // Construcción manual del template
        let rules = grammar.map((rule) => rule.accept(this)).join('\n');

        const template = `
module tokenizer
    implicit none

    contains
    function nextSym(input, cursor) result(lexeme)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: cursor
        character(len=:), allocatable :: lexeme
${rules}
        print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    end function nextSym
end module tokenizer
        `;

        return template;
    }

    visitRule(node) {
        return node.expression.accept(this);
    }

    visitChoice(node) {
        return node.expressions.map((expr) => expr.accept(this)).join('\n');
    }

    visitConcatenation(node) {
        return node.expressions.map((expr) => expr.accept(this)).join('\n');
    }

    visitPluck(node) {
        return node.expression.accept(this);
    }

    visitLabel(node) {
        return node.expression.accept(this);
    }

    visitExpression(node) {
        return node.expression.accept(this);
    }

    visitParsingExpression(node) {
        return node.expression.accept(this);
    }

    visitQuantifier(node) {
        throw new Error('Method not implemented.');
    }

    visitGroup(node) {
        throw new Error('Method not implemented.');
    }

    visitString(node) {
        // Construcción manual del código basado en la plantilla
        const val = node.value;
        const length = val.length;
        const offset = length - 1;

        return `
        if ( "${val}" == input(cursor:cursor + ${offset}) ) then
            allocate( character(len=${length}) :: lexeme)
            lexeme = input(cursor:cursor + ${offset})
            cursor = cursor + ${length}
            return
        end if
        `;
    }

    visitRange(node) {
        throw new Error('Method not implemented.');
    }

    visitInputRange(node) {
        throw new Error('Method not implemented.');
    }

    visitIdentifier(node) {
        throw new Error('Method not implemented.');
    }

    visitNumber(node) {
        throw new Error('Method not implemented.');
    }
}

