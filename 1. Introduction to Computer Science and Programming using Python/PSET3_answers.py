def isWordGuessed(secretWord, lettersGuessed):
    '''
    secretWord: string, the word the user is guessing
    lettersGuessed: list, what letters have been guessed so far
    returns: boolean, True if all the letters of secretWord are in lettersGuessed;
      False otherwise
    '''
    # FILL IN YOUR CODE HERE...
    for letter in list(secretWord):
        if letter not in lettersGuessed:
            return False
            break
    return True

def getGuessedWord(secretWord, lettersGuessed):
    '''
    secretWord: string, the word the user is guessing
    lettersGuessed: list, what letters have been guessed so far
    returns: string, comprised of letters and underscores that represents
      what letters in secretWord have been guessed so far.
    '''
    # FILL IN YOUR CODE HERE...
    current = ''
    for letter in list(secretWord):
        if letter in lettersGuessed:
            current += letter
        if letter not in lettersGuessed:
            current += '_'
    return current

def getAvailableLetters(lettersGuessed):
    '''
    lettersGuessed: list, what letters have been guessed so far
    returns: string, comprised of letters that represents what letters have not
      yet been guessed.
    '''
    # FILL IN YOUR CODE HERE...
    import string
    options = string.ascii_lowercase
    for letter in lettersGuessed:
        if letter not in options:
            continue
        else:
           options = options.replace(letter, '')
    return options

def hangman(secretWord):
    '''
    secretWord: string, the secret word to guess.

    Starts up an interactive game of Hangman.

    * At the start of the game, let the user know how many
      letters the secretWord contains.

    * Ask the user to supply one guess (i.e. letter) per round.

    * The user should receive feedback immediately after each guess
      about whether their guess appears in the computers word.

    * After each round, you should also display to the user the
      partially guessed word so far, as well as letters that the
      user has not yet guessed.

    Follows the other limitations detailed in the problem write-up.
    '''
    # FILL IN YOUR CODE HERE...
    print('Welcome to the game, Hangman!\n'
    'I am thinking of a word that is', len(secretWord), 'letters long\n'
    '-----------')
    guesses = 8
    lettersGuessed = []
    while guesses > 0:
        print('You have', guesses, 'guesses left.')
        print('Available letters:', getAvailableLetters(lettersGuessed))
        inp = input('Please guess a letter: ')
        if inp in lettersGuessed:
            print("Oops! You've already guessed that letter:", getGuessedWord(secretWord, lettersGuessed),'\n'
    '-----------')
            continue
        else:
            lettersGuessed.append(inp)
            if isWordGuessed(secretWord, lettersGuessed) is True:
                print('Good guess:', getGuessedWord(secretWord, lettersGuessed),'\n'
    '-----------')
                print('Congratulations, you won!\n'
    '-----------')
                return None
            elif inp in secretWord:
                print('Good guess:', getGuessedWord(secretWord, lettersGuessed),'\n'
    '-----------')
            else:
                guesses -= 1
                print('Oops! That letter is not in my word:', getGuessedWord(secretWord, lettersGuessed),'\n'
    '-----------')
    print('Sorry, you ran out of guesses. The word was', secretWord)
