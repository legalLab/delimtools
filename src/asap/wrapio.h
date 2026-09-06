/*
 * wrapio.h — substituto para R
 * O original redireccionava I/O para Python via PyObject*.
 * Aqui simplesmente removemos os #define que conflituam com stdio.h
 * e deixamos fprintf/printf do C padrão funcionarem normalmente.
 * As mensagens vão para stderr (visível no console R) ou são ignoradas.
 */

#ifndef WRAPIO_H
#define WRAPIO_H

/* Nada a fazer: usamos stdio.h padrão */

#endif /* WRAPIO_H */
