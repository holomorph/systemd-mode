;;; systemd-company.el --- company backend for systemd unit directives -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))

(declare-function company-mode "company")
(declare-function company-begin-backend "company")
(declare-function company-grab-symbol "company")

(defconst systemd-company-unit-sections
  '("Unit" "Install" "Service")
  "Configuration sections for systemd 225.")

(defconst systemd-company-unit-directives
  ;; TODO: keep a script of sorts for generating this list.  systemd
  ;; source has a python script in tools/ for parsing the
  ;; documentation xml for the unit directives.
  ;;
  ;; forcer on freenode threw together a curl monstrosity for achieving
  ;; the same:
  ;; curl -s http://www.freedesktop.org/software/systemd/man/systemd.directives.html | tr -d '\n' | sed 's/>/>\n/g' | sed -ne '/Unit directives/,/Options on the kernel/p' | sed -ne 's/.*<dt id="\([^-][^"]*\)=">.*/\1/p'
  ;; Quote, wrap with fill-column at 72, insert into list and indent
  '("Accept" "AccuracySec" "After" "Alias" "AllowIsolate" "Also"
    "AppArmorProfile" "AssertACPower" "AssertArchitecture"
    "AssertCapability" "AssertDirectoryNotEmpty" "AssertFileIsExecutable"
    "AssertFileNotEmpty" "AssertFirstBoot" "AssertHost"
    "AssertKernelCommandLine" "AssertNeedsUpdate" "AssertPathExists"
    "AssertPathExistsGlob" "AssertPathIsDirectory" "AssertPathIsMountPoint"
    "AssertPathIsReadWrite" "AssertPathIsSymbolicLink" "AssertSecurity"
    "AssertVirtualization" "Backlog" "Before" "BindIPv6Only" "BindToDevice"
    "BindsTo" "BlockIOAccounting" "BlockIODeviceWeight"
    "BlockIOReadBandwidth" "BlockIOWeight" "BlockIOWriteBandwidth"
    "Broadcast" "BusName" "BusPolicy" "CPUAccounting" "CPUAffinity"
    "CPUQuota" "CPUSchedulingPolicy" "CPUSchedulingPriority"
    "CPUSchedulingResetOnFork" "CPUShares" "Capabilities"
    "CapabilityBoundingSet" "ConditionACPower" "ConditionArchitecture"
    "ConditionCapability" "ConditionDirectoryNotEmpty"
    "ConditionFileIsExecutable" "ConditionFileNotEmpty" "ConditionFirstBoot"
    "ConditionHost" "ConditionKernelCommandLine" "ConditionNeedsUpdate"
    "ConditionPathExists" "ConditionPathExistsGlob"
    "ConditionPathIsDirectory" "ConditionPathIsMountPoint"
    "ConditionPathIsReadWrite" "ConditionPathIsSymbolicLink"
    "ConditionSecurity" "ConditionVirtualization" "Conflicts"
    "DefaultDependencies" "DefaultInstance" "DeferAcceptSec" "Delegate"
    "Description" "DeviceAllow" "DevicePolicy" "DirectoryMode"
    "DirectoryNotEmpty" "Documentation" "Environment" "EnvironmentFile"
    "ExecReload" "ExecStart" "ExecStartPost" "ExecStartPre" "ExecStop"
    "ExecStopPost" "ExecStopPre" "FailureAction" "FileDescriptorName"
    "FileDescriptorStoreMax" "FreeBind" "Group" "GuessMainPID"
    "IOSchedulingClass" "IOSchedulingPriority" "IPTOS" "IPTTL"
    "IgnoreOnIsolate" "IgnoreSIGPIPE" "InaccessibleDirectories"
    "JobTimeoutAction" "JobTimeoutRebootArgument" "JobTimeoutSec"
    "JoinsNamespaceOf" "KeepAlive" "KeepAliveIntervalSec" "KeepAliveProbes"
    "KeepAliveTimeSec" "KillMode" "KillSignal" "LimitAS" "LimitCORE"
    "LimitCPU" "LimitDATA" "LimitFSIZE" "LimitLOCKS" "LimitMEMLOCK"
    "LimitMSGQUEUE" "LimitNICE" "LimitNOFILE" "LimitNPROC" "LimitRSS"
    "LimitRTPRIO" "LimitRTTIME" "LimitSIGPENDING" "LimitSTACK"
    "ListenDatagram" "ListenFIFO" "ListenMessageQueue" "ListenNetlink"
    "ListenSequentialPacket" "ListenSpecial" "ListenStream"
    "ListenUSBFunction" "MakeDirectory" "Mark" "MaxConnections"
    "MemoryAccounting" "MemoryLimit" "MessageQueueMaxMessages"
    "MessageQueueMessageSize" "MountFlags" "NetClass" "Nice" "NoDelay"
    "NoNewPrivileges" "NonBlocking" "NotifyAccess" "OOMScoreAdjust"
    "OnActiveSec" "OnBootSec" "OnCalendar" "OnFailure" "OnFailureJobMode"
    "OnStartupSec" "OnUnitActiveSec" "OnUnitInactiveSec" "Options" "PAMName"
    "PIDFile" "PartOf" "PassCredentials" "PassEnvironment" "PassSecurity"
    "PathChanged" "PathExists" "PathExistsGlob" "PathModified"
    "PermissionsStartOnly" "Persistent" "Personality" "PipeSize" "Priority"
    "PrivateDevices" "PrivateNetwork" "PrivateTmp" "PropagatesReloadTo"
    "ProtectHome" "ProtectSystem" "RandomSec" "ReadOnlyDirectories"
    "ReadWriteDirectories" "RebootArgument" "ReceiveBuffer"
    "RefuseManualStart" "RefuseManualStop" "ReloadPropagatedFrom"
    "RemainAfterElapse" "RemainAfterExit" "RemoveOnStop" "RequiredBy"
    "Requires" "RequiresMountsFor" "Requisite" "Restart"
    "RestartForceExitStatus" "RestartPreventExitStatus" "RestartSec"
    "RestrictAddressFamilies" "ReusePort" "RootDirectory"
    "RootDirectoryStartOnly" "RuntimeDirectory" "RuntimeDirectoryMode"
    "SELinuxContext" "SELinuxContextFromNet" "SecureBits" "SendBuffer"
    "SendSIGHUP" "SendSIGKILL" "Service" "Slice" "SloppyOptions"
    "SmackLabel" "SmackLabelIPIn" "SmackLabelIPOut" "SmackProcessLabel"
    "SocketGroup" "SocketMode" "SocketProtocol" "SocketUser" "Sockets"
    "SourcePath" "StandardError" "StandardInput" "StandardOutput"
    "StartLimitAction" "StartLimitBurst" "StartLimitInterval"
    "StartupBlockIOWeight" "StartupCPUShares" "StopWhenUnneeded"
    "SuccessExitStatus" "SupplementaryGroups" "Symlinks" "SyslogFacility"
    "SyslogIdentifier" "SyslogLevel" "SyslogLevelPrefix"
    "SystemCallArchitectures" "SystemCallErrorNumber" "SystemCallFilter"
    "TCPCongestion" "TTYPath" "TTYReset" "TTYVHangup" "TTYVTDisallocate"
    "TasksAccounting" "TasksMax" "TimeoutIdleSec" "TimeoutSec"
    "TimeoutStartSec" "TimeoutStopSec" "TimerSlackNSec" "Transparent" "Type"
    "UMask" "USBFunctionDescriptors" "USBFunctionStrings" "Unit" "User"
    "UtmpIdentifier" "UtmpMode" "WakeSystem" "WantedBy" "Wants"
    "WatchdogSec" "What" "Where" "WorkingDirectory" "Writable")
  "Configuration directives for systemd 228.")

(defconst systemd-company-network-sections
  '("Match" "Link" "NetDev" "VLAN" "MACVLAN" "MACVTAP" "IPVLAN" "VXLAN"
    "Tunnel" "Peer" "Tun" "Tap" "Bond" "Network" "Address" "Route" "DHCP"
    "Bridge" "BridgeFDB")
  "Network configuration sections for systemd 225.")

(defconst systemd-company-network-directives
  ;; /Network directives/,/Journal fields/p
  '("ARPAllTargets" "ARPIPTargets" "ARPIntervalSec" "ARPProxy" "ARPValidate"
    "AdSelect" "Address" "AllSlavesActive" "AllowPortToBeRoot"
    "Architecture" "BindCarrier" "BitsPerSecond" "Bond" "Bridge"
    "ClientIdentifier" "CopyDSCP" "Cost" "CriticalConnection" "DHCP"
    "DHCPServer" "DNS" "DefaultLeaseTimeSec" "Description" "Destination" "DiscoverPathMTU"
    "Domains" "DownDelaySec" "Driver" "Duplex" "EmitDNS" "EmitNTP"
    "EmitTimezone" "EncapsulationLimit" "FDBAgeingSec" "FailOverMACPolicy"
    "FallbackDNS" "FallbackNTP" "FastLeave" "ForwardDelaySec" "Gateway"
    "GratuitousARP" "GroupPolicyExtension" "HairPin" "HelloTimeSec" "Host"
    "Hostname" "IPForward" "IPMasquerade" "IPv4LLRoute"
    "IPv6AcceptRouterAdvertisements" "IPv6DuplicateAddressDetection"
    "IPv6FlowLabel" "IPv6HopLimit" "IPv6PrivacyExtensions" "IPv6Token" "Id"
    "KernelCommandLine" "Kind" "L2MissNotification" "L3MissNotification"
    "LACPTransmitRate" "LLDP" "LLMNR" "Label" "LearnPacketIntervalSec"
    "LinkLocalAddressing" "Local" "MACAddress" "MACAddressPolicy" "MACVLAN"
    "MIIMonitorSec" "MTUBytes" "MacLearning" "MaxAgeSec" "MaxLeaseTimeSec"
    "MaximumFDBEntries" "Metric" "MinLinks" "Mode" "MultiQueue" "NTP" "Name"
    "NamePolicy" "OneQueue" "OriginalName" "PacketInfo" "PacketsPerSlave"
    "Path" "Peer" "PoolOffset" "PoolSize" "PreferredSource"
    "PrimaryReselectPolicy" "Remote" "RequestBroadcast" "ResendIGMP"
    "RouteMetric" "RouteShortCircuit" "Scope" "SendHostname" "Source" "TOS"
    "TTL" "Timezone" "TransmitHashPolicy" "Tunnel" "UDP6ZeroCheckSumRx"
    "UDP6ZeroChecksumTx" "UDPCheckSum" "UnicastFlood" "UpDelaySec" "UseBPDU"
    "UseDNS" "UseDomains" "UseHostname" "UseMTU" "UseNTP" "UseRoutes"
    "UseTimezone" "VLAN" "VLANId" "VNetHeader" "VXLAN"
    "VendorClassIdentifier" "Virtualization" "WakeOnLan")
  "Network configuration directives for systemd 228.")

(defun systemd-company--setup (enable)
  (when (fboundp 'systemd-company--setup-company)
    (systemd-company--setup-company enable)))

(defun systemd-company-section-p ()
  "Return t if current line begins with \"[\", otherwise nil."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\[")))

(defun systemd-company-network-p ()
  "Return non-nil if `buffer-name' has a network-type extension, otherwise nil."
  (string-match "\\.\\(link\\|netdev\\|network\\)\\'" (buffer-name)))

(with-eval-after-load "company"
  (defun systemd-company-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'systemd-company-backend))
      (prefix (and (eq major-mode 'systemd-mode)
                   (company-grab-symbol)))
      (candidates
       (cl-loop
        for comp in (if (systemd-company-network-p)
                        (if (systemd-company-section-p)
                            systemd-company-network-sections
                          systemd-company-network-directives)
                      (if (systemd-company-section-p)
                          systemd-company-unit-sections
                        systemd-company-unit-directives))
        if (string-prefix-p arg comp) collect comp))))
  (defun systemd-company--setup-company (enable)
    (when enable
      (add-to-list (make-local-variable 'company-backends) 'systemd-company-backend))
    (company-mode (if enable 1 -1))))

(provide 'systemd-company)

;;; systemd-company.el ends here
