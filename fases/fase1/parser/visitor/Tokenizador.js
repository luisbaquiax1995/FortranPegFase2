import Visitor from './Visitor.js';

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
        return node.expr.accept(this);
    }
    visitString(node) {
        const templateInsensitive = `
        if ( tolower("${node.val}") == tolower(input(cursor:cursor + ${
            node.val.length - 1
        } ))) then
            allocate( character(len=${node.val.length}) :: lexeme)
            lexeme = input(cursor:cursor + ${node.val.length - 1})
            cursor = cursor + ${node.val.length}
            return
        end if
        `;

        const template = `
        if ( "${node.val}" == input(cursor:cursor + ${
            node.val.length - 1
        } )) then
            allocate( character(len=${node.val.length}) :: lexeme)
            lexeme = input(cursor:cursor + ${node.val.length - 1})
            cursor = cursor + ${node.val.length}
            return
        end if
        `;

        return node.isCase ? templateInsensitive : template;
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
        return node.exprs.map((expr) => expr.accept(this)).join('\n');
    }

    visitrango(node) {
        return `
    if (cursor <= len_trim(input)) then
        if (iachar(input(cursor:cursor)) >= iachar("${node.start}") .and. &
            iachar(input(cursor:cursor)) <= iachar("${node.end}")) then
            allocate( character(len=1) :: lexeme)
            lexeme = input(cursor:cursor)
            cursor = cursor + 1
            return
        end if
    end if
    `;
    }

    visitliteralRango(node) {
        const literalMap = {
            "\\t": "char(9)",  // Tabulación
            "\\n": "char(10)", // Nueva línea
            " ": "char(32)",   // Espacio
            "\r": "char(13)",  // Retorno de carro
        };
    
        // Verifica si el literal es especial y tradúcelo, de lo contrario usa comillas
        const literalFortran = literalMap[node.val] || `"${node.val}"`;
    
        return `
        if (input(cursor:cursor) == ${literalFortran}) then
            allocate( character(len=1) :: lexeme)
            lexeme = input(cursor:cursor)
            cursor = cursor + 1
            return
        end if
        `;
    }

    visitidRel(node) {
        return ``;
    }

    visitgrupo(node) {
        return node.expr.accept(this);
    }

}
