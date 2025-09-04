# Manuel Utilisateur - Application Comptable Python

## 🎯 **Guide d'utilisation pour utilisateurs non-techniques**

### 📋 Table des matières
1. [Introduction](#introduction)
2. [Première connexion](#première-connexion)
3. [Interface utilisateur](#interface-utilisateur)
4. [Fonctionnalités principales](#fonctionnalités-principales)
5. [Cas d'usage courants](#cas-dusage-courants)
6. [Résolution de problèmes](#résolution-de-problèmes)
7. [Support](#support)

## 📖 Introduction

L'application comptable Python permet de gérer facilement les opérations financières de base :
- **Consultation du solde** de votre compte
- **Ajout de fonds** (crédit)
- **Retrait de fonds** (débit)
- **Historique des transactions**

### Qui peut utiliser cette application ?
- ✅ Comptables et gestionnaires financiers
- ✅ Responsables de trésorerie
- ✅ Directeurs administratifs et financiers
- ✅ Auditeurs internes

## 🔐 Première connexion

### Étape 1 : Accès à l'application
1. Ouvrez votre navigateur web
2. Rendez-vous sur : `https://votre-domaine.com`
3. Vous arrivez sur l'écran de connexion

### Étape 2 : Authentification
```
📧 Email : votre.email@entreprise.com
🔒 Mot de passe : [fourni par l'administrateur]
```

### Étape 3 : Premier paramétrage
1. Changez votre mot de passe initial
2. Configurez votre profil utilisateur
3. Vérifiez vos permissions

## 🖥️ Interface utilisateur

### Menu principal
```
┌─────────────────────────────────────┐
│  🏦 Application Comptable Python    │
├─────────────────────────────────────┤
│  1. 📊 Consulter le solde           │
│  2. ➕ Créditer le compte           │
│  3. ➖ Débiter le compte            │
│  4. 📋 Historique                   │
│  5. ⚙️  Paramètres                  │
│  6. 🚪 Quitter                      │
└─────────────────────────────────────┘
```

### Navigation
- **Flèches ⬆️⬇️** : Navigation dans les menus
- **Entrée ⏎** : Validation d'une sélection
- **Échap** : Retour au menu précédent
- **Ctrl+C** : Sortie d'urgence

## ⚡ Fonctionnalités principales

### 1. 📊 Consultation du solde

**Objectif :** Afficher le solde actuel du compte

**Étapes :**
1. Sélectionnez "Consulter le solde" (option 1)
2. Le solde s'affiche immédiatement
3. Appuyez sur Entrée pour revenir au menu

**Exemple d'affichage :**
```
╔══════════════════════════════════════╗
║           SOLDE ACTUEL               ║
║                                      ║
║        💰 1 250,75 €                 ║
║                                      ║
║  Dernière mise à jour : 02/09/2025   ║
╚══════════════════════════════════════╝
```

### 2. ➕ Crédit du compte (Ajout de fonds)

**Objectif :** Ajouter de l'argent sur le compte

**Étapes :**
1. Sélectionnez "Créditer le compte" (option 2)
2. Saisissez le montant à ajouter
3. Confirmez l'opération
4. Vérifiez le nouveau solde

**Exemple :**
```
💳 Crédit du compte
Montant à créditer : 500.00
✅ Confirmer ? (o/n) : o

✅ Opération réussie !
Ancien solde : 1 250,75 €
Nouveau solde : 1 750,75 €
```

### 3. ➖ Débit du compte (Retrait de fonds)

**Objectif :** Retirer de l'argent du compte

**Étapes :**
1. Sélectionnez "Débiter le compte" (option 3)
2. Saisissez le montant à retirer
3. Le système vérifie les fonds disponibles
4. Confirmez l'opération

**Exemple avec fonds suffisants :**
```
💸 Débit du compte
Montant à débiter : 300.00
Fonds disponibles : ✅ Suffisants
✅ Confirmer ? (o/n) : o

✅ Opération réussie !
Ancien solde : 1 750,75 €
Nouveau solde : 1 450,75 €
```

**Exemple avec fonds insuffisants :**
```
💸 Débit du compte
Montant à débiter : 2000.00
❌ ERREUR : Fonds insuffisants
Solde actuel : 1 450,75 €
Montant demandé : 2 000,00 €
Découvert autorisé : 0,00 €
```

## 💼 Cas d'usage courants

### Scenario 1 : Vérification quotidienne du solde
**Contexte :** Début de journée de travail
1. Connexion à l'application
2. Consultation du solde
3. Vérification des dernières transactions
4. Rapport au responsable si nécessaire

### Scenario 2 : Réception d'un paiement client
**Contexte :** Paiement de facture reçu
1. Calcul du montant à créditer
2. Crédit du compte avec le montant exact
3. Vérification du nouveau solde
4. Documentation de l'opération

### Scenario 3 : Paiement fournisseur
**Contexte :** Règlement d'une facture fournisseur
1. Vérification du solde disponible
2. Débit du compte pour le montant de la facture
3. Contrôle du solde restant
4. Archivage de la transaction

### Scenario 4 : Fin de mois
**Contexte :** Clôture comptable mensuelle
1. Consultation du solde final
2. Vérification de l'historique du mois
3. Export des données si nécessaire
4. Rapport de clôture

## 🔧 Résolution de problèmes

### Problèmes de connexion

**Problème :** "Mot de passe incorrect"
- ✅ Vérifiez la casse (majuscules/minuscules)
- ✅ Utilisez le mot de passe temporaire si premier accès
- ✅ Contactez l'administrateur si oubli

**Problème :** "Utilisateur verrouillé"
- ❓ Trop de tentatives incorrectes
- 🔧 Attendre 15 minutes OU contacter l'admin

### Problèmes d'opérations

**Problème :** "Montant invalide"
```
❌ Montant invalide : abc
✅ Format correct : 123.45
✅ Utilisez le point (.) pour les décimales
✅ Maximum 2 décimales
```

**Problème :** "Opération rejetée"
- 🔍 Vérifiez vos permissions utilisateur
- 🔍 Vérifiez les limites de votre compte
- 📞 Contactez le support si persistant

### Problèmes techniques

**Problème :** Application lente
- 🌐 Vérifiez votre connexion internet
- 🔄 Rafraîchissez la page (F5)
- 🚪 Déconnexion/reconnexion

**Problème :** Écran figé
- ⌨️ Appuyez sur Échap
- 🔄 Redémarrez l'application
- 📞 Contactez le support technique

## 📊 Bonnes pratiques

### Sécurité
- 🔒 **Toujours fermer** l'application après usage
- 🔒 **Ne jamais partager** vos identifiants
- 🔒 **Changer votre mot de passe** régulièrement
- 🔒 **Signaler** toute activité suspecte

### Utilisation
- ✅ **Vérifiez toujours** le montant avant validation
- ✅ **Documentez** vos opérations importantes
- ✅ **Faites des sauvegardes** des données critiques
- ✅ **Formez-vous** régulièrement aux nouvelles fonctionnalités

### Performance
- ⚡ **Fermez** les autres applications gourmandes
- ⚡ **Utilisez** Chrome ou Firefox récents
- ⚡ **Évitez** les opérations simultanées multiples

## 📞 Support et assistance

### Niveaux de support

#### Niveau 1 : Auto-assistance
- 📖 Consultez ce manuel
- 🔍 Utilisez la fonction "Aide" intégrée
- 💡 Consultez la FAQ en ligne

#### Niveau 2 : Support utilisateur
- 📧 Email : support@entreprise.com
- 📞 Téléphone : +33 1 23 45 67 89
- 🕐 Horaires : 8h-18h du lundi au vendredi

#### Niveau 3 : Support technique
- 📧 Email : tech@entreprise.com
- 🆘 Urgences : +33 6 12 34 56 78
- 🕐 Disponibilité : 24h/7j pour les urgences

### Informations à préparer lors d'un contact
1. **Votre identifiant utilisateur**
2. **Description précise du problème**
3. **Étapes pour reproduire l'erreur**
4. **Capture d'écran si possible**
5. **Heure et date de l'incident**

### FAQ (Foire Aux Questions)

**Q : Puis-je utiliser l'application sur mobile ?**
R : Actuellement, l'application est optimisée pour ordinateur. Une version mobile est en développement.

**Q : Combien de temps sont conservées les données ?**
R : Les données sont conservées 7 ans conformément à la réglementation comptable.

**Q : Puis-je exporter mes données ?**
R : Oui, fonction d'export disponible dans les paramètres avancés.

**Q : Y a-t-il une limite au nombre d'opérations ?**
R : Non, aucune limite technique. Seules s'appliquent les règles métier de votre organisation.

---

*Ce manuel est mis à jour régulièrement. Version actuelle : 1.0 - Septembre 2025*
