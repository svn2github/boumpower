#include <iostream>
using namespace std;

/* Console Windows améliorée
   Il faut renomer le main() en execute()
   et compiler le programme pour Windows...
   
   Pour améliorer l'ergonomie, on peut créer
   un template à partir de ce projet :
   Menu:Fichier:Nouveau:Template... */
   
void execute(){
  std::string str; 
    
  cout << "Sortie sur console" << endl;
  cerr << "Affiche un message d'erreur" << endl;
  clog << "Affiche un message\r\npar exemple l'évolution de la mémoire" << endl;
  cin >> str;
  cout << "Entrée au clavier : " << str << endl;     
  cout << "Fin Test" << endl;
}     
