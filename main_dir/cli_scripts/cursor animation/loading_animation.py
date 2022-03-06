"""
A cli script to animate cursor after hititng enter

Maintaier: Kyle Gortych
Date:      01-19-2022
"""
import time

def cursor_animate(text, delay=0.1):
    """
    Appends animation next to cli comands
    after hitting enter. Should run
    at the same time as cli comand.
    Retruns empty string after animnation?

    params: letter (char): arg |
    ------- text   (str) : arg |

    invar:  delay  (int) :
    ------

    precondition:
    -------------

    return: string | empty?
    -------
    """
    for letter in text:
        print(letter, end='', flush=True)
        time.sleep(delay)
    print()

cursor_animate(">>>>>", 0.09)
