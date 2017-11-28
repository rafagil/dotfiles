[channing@nixos:/home/channing]$ cat /etc/nixos/configuration.nix  
# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    vim                                                                                                                                                              
    chromium                                                                                                                                                         
    emacs                                                                                                                                                            
    gitFull                                                                                                                                                          
    jdk                                                                                                                                                              
    sbt                                                                                                                                                              
    unzip                                                                                                                                                            
    gnupg                                                                                                                                                            
    thunderbird                                                                                                                                                      
  ];                                                                                                                                                                 
                                                                                                                                                                     
  # Some programs need SUID wrappers, can be configured further or are                                                                                               
  # started in user sessions.                                                                                                                                        
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;
  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  # VirtualBox Guest Additions
  virtualisation.virtualbox.guest.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.channing = {
    isNormalUser = true;
    home="/home/channing/";
    description="Channing Walton";
    extraGroups=[ "wheel" "networkmanager" ];
  };

  nixpkgs.config.allowUnfree = true;

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      #anonymousPro
      comfortaa
      comic-neue
      comic-relief
      corefonts
      #dejavu_fonts
      fira
      fira-code
      fira-mono
      font-droid
      google-fonts
      #inconsolata
      #iosevka
      source-code-pro
      source-sans-pro
      source-serif-pro
      #ttf_bitstream_vera
      vistafonts
    ];
    fontconfig.defaultFonts = {
      monospace = ["Fira Code"];
      sansSerif = ["Roboto"];
      serif = ["Roboto Slab"];
    };
    fontconfig.ultimate.substitutions = "combi";
  };

  system.autoUpgrade.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

}



