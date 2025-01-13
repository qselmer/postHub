import termcolor
import random
import time
import sys
import os
from colorama import init
from termcolor import colored

# Limpia la pantalla
def clear_screen():
    if sys.platform == 'win32':
        os.system('cls')
    else:
        os.system('clear')

# Genera una fila del árbol con luces y estrellas
def generate_tree_row(row_length, lights, default_char='*'):
    row = ''
    for _ in range(row_length):
        if random.random() < 0.4:  # 40% probabilidad de luz
            row += random.choice(lights)
        else:
            row += termcolor.colored(default_char, 'green', attrs=['bold'])
    return row

# Marco decorativo
def draw_border(width):
    border = termcolor.colored('#' * (width + 4), 'red', attrs=['bold'])
    print(border)

def display_tree():
    init()
    # 
    lights = [
        termcolor.colored('o', 'red', attrs=['bold']),
        termcolor.colored('o', 'yellow', attrs=['bold']),
        termcolor.colored('o', 'cyan', attrs=['bold']),
        termcolor.colored('o', 'magenta', attrs=['bold']),
        termcolor.colored('o', 'blue', attrs=['bold']),
    ]
    # Colores para los mensajes
    text_colors = ['red', 'yellow', 'cyan', 'magenta', 'blue', 'green', 'white']

    # Mensajes fijos en la base del árbol
    footer_message1 = termcolor.colored('📧 Correo: qselmer@gmail.com', 'white', attrs=['bold'])
    footer_message2 = termcolor.colored('🌐 GitHub: github.com/qselmer', 'cyan', attrs=['bold', 'underline'])
    footer_message3 = termcolor.colored('✨ ¡Felicidades! ✨', 'yellow', attrs=['bold'])

    try:
        while True:
            clear_screen()
            width = 30
            draw_border(width)  # Marco superior

            # Estrella en la cima (una sola estrella)
            print(f"{' ' * 15}{termcolor.colored('★', 'yellow', attrs=['bold', 'blink'])}")

            # Genera el árbol
            for i in range(1, width, 2):
                tree_row = generate_tree_row(i, lights)
                padding = ' ' * (15 - i // 2)
                print(f"{padding}{tree_row}{padding}")
            
            # Tronco del árbol
            trunk = termcolor.colored('|||', 'black', attrs=['bold'])
            for _ in range(3):
                print(f"{' ' * 14}{trunk}")

            # Mensajes alternantes
            message1 = termcolor.colored('🎄 ¡Feliz Navidad! 🎄', random.choice(text_colors), attrs=['bold', 'underline'])
            message2 = termcolor.colored('🌊 ¡Próspero Año Nuevo! 🌊', random.choice(text_colors), attrs=['bold', 'underline'])
            message = message1 if random.random() > 0.5 else message2
            print(f"{' ' * 5}{message}")

            # Base del árbol con información fija
            print(f"{' ' * 3}{footer_message1}")
            print(f"{' ' * 3}{footer_message2}")
            print(f"{' ' * 6}{footer_message3}")
            
            # Mensaje de como parar el programa
            stop_message = termcolor.colored('🔴 Detener: Ctrl + C 🔴', 'red', attrs=['bold'])
            print(f"{' ' * 5}{stop_message}")

            draw_border(width)  # Marco inferior

            time.sleep(0.6)  # Pausa antes de refrescar
    except KeyboardInterrupt:
        # Mensaje de despedida
        print("\n¡Adiós! 🎅 Que tengas una Feliz Navidad y un Próspero Año Nuevo 🎄")

# Ejecuta la animación
if __name__ == '__main__':
    display_tree()


    