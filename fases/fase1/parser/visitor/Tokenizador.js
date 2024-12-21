import Visitor from './Visitor.js';
import * as n from './CST.js';

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        return `
module tokenizer
implicit none

contains
function tolower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i

        lower_str = str 
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
end function tolower

function nextSym(input, cursor) result(lexeme)
    character(len=*), intent(in) :: input
    integer, intent(inout) :: cursor
    character(len=:), allocatable :: lexeme
    integer :: initialCursor

    if (cursor > len(input)) then
        allocate( character(len=3) :: lexeme )
        lexeme = "EOF"
        return
    end if

    ${grammar.map((produccion) => produccion.accept(this)).join('\n')}

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module tokenizer 
        `;
    }

    visitProducciones(node) {
        return node.expr.accept(this);
    }
    visitOpciones(node) {
        return node.exprs.map((expr) => expr.accept(this)).join('\n');
    }
    visitUnion(node) {
        return node.exprs.map((expr) => expr.accept(this)).join('\n');
    }

    visitExpresion(node) {
        if ( node.qty && //there is a quantifier
            (node.expr instanceof n.String 
            || node.expr instanceof n.Corchetes)
        ){
            node.expr.qty = node.qty // inherit quantifier
        }
        return node.expr.accept(this);
    }

    visitString(node) {
        const condition = node.isCase 
        ? `tolower("${node.val}") == tolower(input(cursor:cursor + ${ node.val.length - 1} ))`
        :  `"${node.val}" == input(cursor:cursor + ${node.val.length - 1} )`;
        return this.renderQuantifierOption(node.qty, condition, node.val.length)
    }

    visitAny(node) { 
        return `
    ! Cualquier carácter es aceptado como lexema
    if (cursor <= len_trim(input)) then
        allocate( character(len=1) :: lexeme)
        lexeme = input(cursor:cursor)
        cursor = cursor + 1
        return
    end if
    `;
    }

    visitCorchetes(node) {
        node.exprs.forEach(expr => { expr.isCase = node.isCase });
        let conditions = node.exprs.map((expr) => expr.accept(this)).join('& \n    .or. ')
        return this.renderQuantifierOption(node.qty, conditions, 1)
    }

    //Solo devuelve las condiciones a cumplirse
    visitrango(node) {
        const condition = node.isCase 
        ? `iachar(tolower(input(cursor:cursor))) >= iachar("${node.start}") .and. &
        iachar(tolower(input(cursor:cursor))) <= iachar("${node.end}")`
        : `iachar(input(cursor:cursor)) >= iachar("${node.start}") .and. &
        iachar(input(cursor:cursor)) <= iachar("${node.end}")`;

        return "(" + condition + ")";
    }

    //Solo devuelve las condiciones a cumplirse
    visitliteralRango(node) {
        const literalMap = {
            "\\t": "char(9)",  // Tabulación
            "\\n": "char(10)", // Nueva línea
            " ": "char(32)",   // Espacio
            "\r": "char(13)",  // Retorno de carro
        };
    
        // Verifica si el literal es especial y tradúcelo, de lo contrario usa comillas
        const literalFortran = literalMap[node.val] || `"${node.val}"`;
    
        const condition = node.isCase
        ? `tolower(input(cursor:cursor)) == tolower(${literalFortran})`
        : `input(cursor:cursor) == ${literalFortran}`
        return "(" + condition + ")";
    }

    visitidRel(node) {
        return ``;
    }

    visitgrupo(node) {
        return node.expr.accept(this);
    }

    renderQuantifierOption(qty, condition, length){
        return (qty == '+' || qty == '*')
        ? `
    initialCursor = cursor
    do while (cursor <= len_trim(input) .and. (${condition}))
        cursor = cursor + ${length}
    end do
    if (cursor > initialCursor) then
        allocate(character(len=cursor-initialCursor)::lexeme)
        lexeme = input(initialCursor:cursor-1)
        return
    end if`      
            : `
    if (cursor <= len_trim(input) .and. (${condition})) then 
        allocate( character(len=${length}) :: lexeme)
        lexeme = input(cursor:cursor + ${length - 1})
        cursor = cursor + ${length}
        return
    end if`;
    }

}
