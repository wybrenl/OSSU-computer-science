def getWordScore(word, n):
    """
    Returns the score for a word. Assumes the word is a valid word.

    The score for a word is the sum of the points for letters in the
    word, multiplied by the length of the word, PLUS 50 points if all n
    letters are used on the first turn.

    Letters are scored as in Scrabble; A is worth 1, B is worth 3, C is
    worth 3, D is worth 2, E is worth 1, and so on (see SCRABBLE_LETTER_VALUES)

    word: string (lowercase letters)
    n: integer (HAND_SIZE; i.e., hand size required for additional points)
    returns: int >= 0
    """
    score = 0
    for letter in word:
        score += SCRABBLE_LETTER_VALUES[letter]
    if len(word) == n:
        score = score * len(word) + 50
    else:
        score = score * len(word)
    return score

def getFrequencyDict(sequence):
    """
    Returns a dictionary where the keys are elements of the sequence
    and the values are integer counts, for the number of times that
    an element is repeated in the sequence.

    sequence: string or list
    return: dictionary
    """
    # freqs: dictionary (element_type -> int)
    freq = {}
    for x in sequence:
        freq[x] = freq.get(x,0) + 1
    return freq

def updateHand(hand, word):
    """
    Assumes that 'hand' has all the letters in word.
    In other words, this assumes that however many times
    a letter appears in 'word', 'hand' has at least as
    many of that letter in it.

    Updates the hand: uses up the letters in the given word
    and returns the new hand, without those letters in it.

    Has no side effects: does not modify hand.

    word: string
    hand: dictionary (string -> int)
    returns: dictionary (string -> int)
    """
    # TO DO ... <-- Remove this comment when you code this function
    new_hand = {}
    dict_word = getFrequencyDict(word)
    for key, value in hand.items():
        new_hand[key] = value - dict_word.get(key, 0)
    return new_hand

import copy

def isValidWord(word, hand, wordList):
    """
    Returns True if word is in the wordList and is entirely
    composed of letters in the hand. Otherwise, returns False.

    Does not mutate hand or wordList.

    word: string
    hand: dictionary (string -> int)
    wordList: list of lowercase strings
    """
    temp_hand = hand.copy()
    word_check = False
    if word in wordList:
        word_check = True

    #print(word_check)

    letters_in_hand = None

    #print(temp_hand)
    for letters in word:
     #   print(letters)
        if letters not in temp_hand:
            letters_in_hand = False
            break
      #      print('False')
        else:
            if temp_hand[letters] == 1:
                del temp_hand[letters]
            else:
                temp_hand[letters] -= 1
       #     print(temp_hand)
            letters_in_hand = True

    return word_check and letters_in_hand

def calculateHandlen(hand):
    """
    Returns the length (number of letters) in the current hand.

    hand: dictionary (string int)
    returns: integer
    """
    count = 0
    for freq in hand.values():
        count += freq
    return int(count)

def playHand(hand, wordList, n):
    """
    Allows the user to play the given hand, as follows:

    * The hand is displayed.
    * The user may input a word or a single period (the string ".")
      to indicate they're done playing
    * Invalid words are rejected, and a message is displayed asking
      the user to choose another word until they enter a valid word or "."
    * When a valid word is entered, it uses up letters from the hand.
    * After every valid word: the score for that word is displayed,
      the remaining letters in the hand are displayed, and the user
      is asked to input another word.
    * The sum of the word scores is displayed when the hand finishes.
    * The hand finishes when there are no more unused letters or the user
      inputs a "."

      hand: dictionary (string -> int)
      wordList: list of lowercase strings
      n: integer (HAND_SIZE; i.e., hand size required for additional points)

    """
    score = 0
    total_score = 0
    n = calculateHandlen(hand)


    while calculateHandlen(hand) != 0:
        print('Current Hand:', end=' '); displayHand(hand)
        word = input('Enter word, or a "." to indicate that you are finished: ')
        if word == '.':
            break
        elif isValidWord(word, hand, wordList) == False:
            print('Invalid word, please try again.\n')
            continue
        else:
            score = getWordScore(word, n)
            total_score += score
            print("'",word,"'", 'earned', score, 'points.', 'Total:', total_score, 'points\n')
            hand = updateHand(hand, word)

    if word == '.':
        print('Run out of letters. Total score:', total_score, 'points.')
    else:
        print('Goodbye! Total score:', total_score, 'points.')

def playGame(wordList):
    """
    Allow the user to play an arbitrary number of hands.

    1) Asks the user to input 'n' or 'r' or 'e'.
      * If the user inputs 'n', let the user play a new (random) hand.
      * If the user inputs 'r', let the user play the last hand again.
      * If the user inputs 'e', exit the game.
      * If the user inputs anything else, tell them their input was invalid.

    2) When done playing the hand, repeat from step 1
    """
    n = HAND_SIZE
    hand = None

    while True:
        try:
            game = input('Enter n to deal a new hand, r to replay the last hand, or e to end game: ')
            if game == 'n':
                hand = dealHand(n)
                playHand(hand, wordList, HAND_SIZE)
                print('\n')
            elif game == 'r':
                if hand == None:
                    print('You have not played a hand yet. Please play a new hand first!\n')
                    playHand(hand, wordList, HAND_SIZE)
                else:
                    playHand(hand, wordList, HAND_SIZE)
            elif game == 'e':
                break
            else:
                print('Invalid command')
        except:
            continue

def playGame(wordList):
    """
    Allow the user to play an arbitrary number of hands.

    1) Asks the user to input 'n' or 'r' or 'e'.
        * If the user inputs 'e', immediately exit the game.
        * If the user inputs anything that's not 'n', 'r', or 'e', keep asking them again.

    2) Asks the user to input a 'u' or a 'c'.
        * If the user inputs anything that's not 'c' or 'u', keep asking them again.

    3) Switch functionality based on the above choices:
        * If the user inputted 'n', play a new (random) hand.
        * Else, if the user inputted 'r', play the last hand again.
          But if no hand was played, output "You have not played a hand yet.
          Please play a new hand first!"

        * If the user inputted 'u', let the user play the game
          with the selected hand, using playHand.
        * If the user inputted 'c', let the computer play the
          game with the selected hand, using compPlayHand.

    4) After the computer or user has played the hand, repeat from step 1

    wordList: list (string)
    """
    while True:
        user_input = input('Enter n to deal a new hand, r to replay the last hand, or e to end game: ')
        if user_input == 'e':
            break
        elif user_input == 'n':
            while True:
                who_play = input('Enter u to have yourself play, c to have the computer play: ')
                if who_play == 'u':
                    hand = dealHand(HAND_SIZE)
                    playHand(hand, wordList, HAND_SIZE)
                    break
                elif who_play == 'c':
                    hand = dealHand(HAND_SIZE)
                    compPlayHand(hand, wordList, HAND_SIZE)
                    break
                else:
                    print('Invalid command.')
        elif user_input == 'r':
            try:
                temp_hand = hand.copy()
                who_play = input('Enter u to have yourself play, c to have the computer play: ')
                if who_play == 'u':
                    playHand(hand, wordList, HAND_SIZE)
                elif who_play == 'c':
                    compPlayHand(hand, wordList, HAND_SIZE)
                else:
                    print('Invalid command.')
                continue
            except:
                  print('You have not played a hand yet. Please play a new hand first!')
        else:
            print('Invalid command.')
