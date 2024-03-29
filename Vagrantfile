# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure("2") do |config|
  # The most common configuration options are documented and commented below.
  # For a complete reference, please see the online documentation at
  # https://docs.vagrantup.com.

  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://atlas.hashicorp.com/search.
  config.vm.box = "fpco/hashi-stack-09-02-2019"

  # be explicit about the vm's hostname
  config.vm.hostname = "ubuntu-xenial"

  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # NOTE: This will enable public access to the opened port
  # config.vm.network "forwarded_port", guest: 80, host: 8080

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine and only allow access
  # via 127.0.0.1 to disable public access
  #config.vm.network "forwarded_port", guest: 80, host: 8080, host_ip: "127.0.0.1"
  
  # hello world
  #config.vm.network "forwarded_port", guest: 8080, host: 8080, host_ip: "127.0.0.1"
  # consul
  config.vm.network "forwarded_port", guest: 8500, host: 8500, host_ip: "127.0.0.1"
  # vault
  config.vm.network "forwarded_port", guest: 8200, host: 8200, host_ip: "127.0.0.1"
  config.vm.network "forwarded_port", guest: 8201, host: 8201, host_ip: "127.0.0.1"
  # nomad
  config.vm.network "forwarded_port", guest: 4646, host: 4646, host_ip: "127.0.0.1"
  # prometheus
  #config.vm.network "forwarded_port", guest: 9090, host: 9090, host_ip: "127.0.0.1"
  # grafana
  #config.vm.network "forwarded_port", guest: 3000, host: 3000, host_ip: "127.0.0.1"
  # hashi-ui
  config.vm.network "forwarded_port", guest: 5000, host: 5000, host_ip: "127.0.0.1"
  # node_exporter
  #config.vm.network "forwarded_port", guest: 9100, host: 9100, host_ip: "127.0.0.1"
  # nomad_exporter
  #config.vm.network "forwarded_port", guest: 9172, host: 9172, host_ip: "127.0.0.1"
  # consul_exporter
  #config.vm.network "forwarded_port", guest: 9111, host: 9111, host_ip: "127.0.0.1"
  # fabio
  config.vm.network "forwarded_port", guest: 9999, host: 9999, host_ip: "127.0.0.1"
  # fabio admin
  config.vm.network "forwarded_port", guest: 9998, host: 9998, host_ip: "127.0.0.1"
  # logstash beats
  #config.vm.network "forwarded_port", guest: 5044, host: 5044, host_ip: "127.0.0.1"
  # logstash http
  #config.vm.network "forwarded_port", guest: 9600, host: 9600, host_ip: "127.0.0.1"
  # postgres
  config.vm.network "forwarded_port", guest: 5432, host: 5432, host_ip: "127.0.0.1"
  # steadfast
  #config.vm.network "forwarded_port", guest: 8000, host: 8000, host_ip: "127.0.0.1"

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../data", "/vagrant_data"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
   config.vm.provider "virtualbox" do |vb|
     # Display the VirtualBox GUI when booting the machine
     #vb.gui = true
  
     # Customize the amount of memory on the VM:
     vb.memory = "2560"
   end
  #
  # View the documentation for the provider you are using for more
  # information on available options.

  # Define a Vagrant Push strategy for pushing to Atlas. Other push strategies
  # such as FTP and Heroku are also available. See the documentation at
  # https://docs.vagrantup.com/v2/push/atlas.html for more information.
  # config.push.define "atlas" do |push|
  #   push.app = "YOUR_ATLAS_USERNAME/YOUR_APPLICATION_NAME"
  # end

  # Enable provisioning with a shell script. Additional provisioners such as
  # Puppet, Chef, Ansible, Salt, and Docker are also available. Please see the
  # documentation for more information about their specific syntax and use.
  # config.vm.provision "shell", inline: <<-SHELL
  #   apt-get update
  #   apt-get install -y apache2
  # SHELL

  ## run hashistack tests
  #config.vm.provision "shell", path: "tests/scripts/test-hashistack.sh"
  #config.vm.provision "shell", path: "tests/scripts/init-vault.sh"
  #config.vm.provision "shell", path: "tests/scripts/unseal-vault.sh"
  #config.vm.provision "shell", path: "tests/scripts/create-vault-client-token.sh"
  ##config.vm.provision "shell", path: "tests/scripts/test-nomad-job.sh"
end
