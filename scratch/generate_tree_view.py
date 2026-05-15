import os

def generate_tree_view():
    entrer = input("Veuillez entrer le chemin du fichier source : ")
    numero = input("Veuillez entrer le numero du fichier : ")
    input_file = os.path.expanduser(entrer)
    output_file = os.path.expanduser(f"/home/mazzez/Bureau/R project/Data/Final Version/tree_view_report_{numero}.txt")
    if not os.path.exists(input_file):
        print(f"Erreur : Le fichier d'entrée '{input_file}' n'existe pas. Veuillez d'abord exécuter generate_final_report.py.")
        return

    # Construction de l'arbre
    tree = {}

    with open(input_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    # Ignorer les lignes d'en-tête si elles existent (celles qui ne sont pas des chemins)
    # On suppose que les lignes utiles contiennent des chemins relatifs ou des noms de fichiers
    path_lines = [line.strip() for line in lines if line.strip() and not line.startswith("Contenu du dossier") and not line.startswith("=")]

    for path in path_lines:
        parts = path.split(os.sep)
        current_level = tree
        for part in parts:
            if part not in current_level:
                current_level[part] = {}
            current_level = current_level[part]

    # Fonction récursive pour l'affichage
    def print_tree(node, prefix=""):
        lines_output = []
        keys = sorted(node.keys())
        for i, key in enumerate(keys):
            # Vérifier si c'est le dernier élément pour adapter (optionnel, mais ici on veut le style |_ )
            # Le style demandé est juste |_ pour tout le monde selon la capture
            
            line_str = f"{prefix}|_{key}"
            lines_output.append(line_str)
            
            # S'il a des enfants, on descend avec une indentation augmentée
            if node[key]:
                # On ajoute 4 espaces pour l'indentation des enfants
                lines_output.extend(print_tree(node[key], prefix + "    "))
        return lines_output

    output_lines = print_tree(tree)

    try:
        with open(output_file, 'w', encoding='utf-8') as f_out:
            f_out.write("Rapport Structure Arborescente\n")
            f_out.write("==============================\n\n")
            # Racine explicite si voulu, ou juste le contenu
            # D'après la capture, on commence directement
            for line in output_lines:
                f_out.write(line + "\n")
        
        print(f"Succès ! La vue arborescente a été générée dans : {output_file}")

    except Exception as e:
        print(f"Une erreur est survenue : {e}")

if __name__ == "__main__":
    generate_tree_view()
