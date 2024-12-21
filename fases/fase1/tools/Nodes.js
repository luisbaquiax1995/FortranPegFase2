const nodes = {
   Producciones: ['id', 'expr', 'alias'],
   Opciones: ['exprs'],
   Union: ['exprs'],
   Expresion: ['expr', 'label', 'qty'],
   String: ['val', 'isCase', 'qty'],
   Any: ['isAny'],
   Corchetes: ['exprs', 'isCase', 'qty'],
   Rango: ['start', 'end', 'isCase'],
   LiteralRango:['val', 'isCase'],
   IdRel:['val'],
   Grupo:['expr']
};

export default nodes;
