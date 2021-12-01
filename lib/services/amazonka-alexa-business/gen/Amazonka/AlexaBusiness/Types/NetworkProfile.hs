{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AlexaBusiness.Types.NetworkProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.NetworkProfile where

import Amazonka.AlexaBusiness.Types.NetworkEapMethod
import Amazonka.AlexaBusiness.Types.NetworkSecurityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The network profile associated with a device.
--
-- /See:/ 'newNetworkProfile' smart constructor.
data NetworkProfile = NetworkProfile'
  { -- | The name of the network profile associated with a device.
    networkProfileName :: Prelude.Maybe Prelude.Text,
    -- | The SSID of the Wi-Fi network.
    ssid :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
    -- WPA2_PSK, WPA_PSK, WEP, or OPEN.
    securityType :: Prelude.Maybe NetworkSecurityType,
    -- | The current password of the Wi-Fi network.
    currentPassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The next, or subsequent, password of the Wi-Fi network. This password is
    -- asynchronously transmitted to the device and is used when the password
    -- of the network changes to NextPassword.
    nextPassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The authentication standard that is used in the EAP framework.
    -- Currently, EAP_TLS is supported.
    eapMethod :: Prelude.Maybe NetworkEapMethod,
    -- | Detailed information about a device\'s network profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The root certificates of your authentication server, which is installed
    -- on your devices and used to trust your authentication server during EAP
    -- negotiation.
    trustAnchors :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ARN of the Private Certificate Authority (PCA) created in AWS
    -- Certificate Manager (ACM). This is used to issue certificates to the
    -- devices.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkProfileName', 'networkProfile_networkProfileName' - The name of the network profile associated with a device.
--
-- 'ssid', 'networkProfile_ssid' - The SSID of the Wi-Fi network.
--
-- 'networkProfileArn', 'networkProfile_networkProfileArn' - The ARN of the network profile associated with a device.
--
-- 'securityType', 'networkProfile_securityType' - The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- 'currentPassword', 'networkProfile_currentPassword' - The current password of the Wi-Fi network.
--
-- 'nextPassword', 'networkProfile_nextPassword' - The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
--
-- 'eapMethod', 'networkProfile_eapMethod' - The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
--
-- 'description', 'networkProfile_description' - Detailed information about a device\'s network profile.
--
-- 'trustAnchors', 'networkProfile_trustAnchors' - The root certificates of your authentication server, which is installed
-- on your devices and used to trust your authentication server during EAP
-- negotiation.
--
-- 'certificateAuthorityArn', 'networkProfile_certificateAuthorityArn' - The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
newNetworkProfile ::
  NetworkProfile
newNetworkProfile =
  NetworkProfile'
    { networkProfileName =
        Prelude.Nothing,
      ssid = Prelude.Nothing,
      networkProfileArn = Prelude.Nothing,
      securityType = Prelude.Nothing,
      currentPassword = Prelude.Nothing,
      nextPassword = Prelude.Nothing,
      eapMethod = Prelude.Nothing,
      description = Prelude.Nothing,
      trustAnchors = Prelude.Nothing,
      certificateAuthorityArn = Prelude.Nothing
    }

-- | The name of the network profile associated with a device.
networkProfile_networkProfileName :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_networkProfileName = Lens.lens (\NetworkProfile' {networkProfileName} -> networkProfileName) (\s@NetworkProfile' {} a -> s {networkProfileName = a} :: NetworkProfile)

-- | The SSID of the Wi-Fi network.
networkProfile_ssid :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_ssid = Lens.lens (\NetworkProfile' {ssid} -> ssid) (\s@NetworkProfile' {} a -> s {ssid = a} :: NetworkProfile)

-- | The ARN of the network profile associated with a device.
networkProfile_networkProfileArn :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_networkProfileArn = Lens.lens (\NetworkProfile' {networkProfileArn} -> networkProfileArn) (\s@NetworkProfile' {} a -> s {networkProfileArn = a} :: NetworkProfile)

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
networkProfile_securityType :: Lens.Lens' NetworkProfile (Prelude.Maybe NetworkSecurityType)
networkProfile_securityType = Lens.lens (\NetworkProfile' {securityType} -> securityType) (\s@NetworkProfile' {} a -> s {securityType = a} :: NetworkProfile)

-- | The current password of the Wi-Fi network.
networkProfile_currentPassword :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_currentPassword = Lens.lens (\NetworkProfile' {currentPassword} -> currentPassword) (\s@NetworkProfile' {} a -> s {currentPassword = a} :: NetworkProfile) Prelude.. Lens.mapping Core._Sensitive

-- | The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
networkProfile_nextPassword :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_nextPassword = Lens.lens (\NetworkProfile' {nextPassword} -> nextPassword) (\s@NetworkProfile' {} a -> s {nextPassword = a} :: NetworkProfile) Prelude.. Lens.mapping Core._Sensitive

-- | The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
networkProfile_eapMethod :: Lens.Lens' NetworkProfile (Prelude.Maybe NetworkEapMethod)
networkProfile_eapMethod = Lens.lens (\NetworkProfile' {eapMethod} -> eapMethod) (\s@NetworkProfile' {} a -> s {eapMethod = a} :: NetworkProfile)

-- | Detailed information about a device\'s network profile.
networkProfile_description :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_description = Lens.lens (\NetworkProfile' {description} -> description) (\s@NetworkProfile' {} a -> s {description = a} :: NetworkProfile)

-- | The root certificates of your authentication server, which is installed
-- on your devices and used to trust your authentication server during EAP
-- negotiation.
networkProfile_trustAnchors :: Lens.Lens' NetworkProfile (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
networkProfile_trustAnchors = Lens.lens (\NetworkProfile' {trustAnchors} -> trustAnchors) (\s@NetworkProfile' {} a -> s {trustAnchors = a} :: NetworkProfile) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
networkProfile_certificateAuthorityArn :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_certificateAuthorityArn = Lens.lens (\NetworkProfile' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@NetworkProfile' {} a -> s {certificateAuthorityArn = a} :: NetworkProfile)

instance Core.FromJSON NetworkProfile where
  parseJSON =
    Core.withObject
      "NetworkProfile"
      ( \x ->
          NetworkProfile'
            Prelude.<$> (x Core..:? "NetworkProfileName")
            Prelude.<*> (x Core..:? "Ssid")
            Prelude.<*> (x Core..:? "NetworkProfileArn")
            Prelude.<*> (x Core..:? "SecurityType")
            Prelude.<*> (x Core..:? "CurrentPassword")
            Prelude.<*> (x Core..:? "NextPassword")
            Prelude.<*> (x Core..:? "EapMethod")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "TrustAnchors")
            Prelude.<*> (x Core..:? "CertificateAuthorityArn")
      )

instance Prelude.Hashable NetworkProfile where
  hashWithSalt salt' NetworkProfile' {..} =
    salt'
      `Prelude.hashWithSalt` certificateAuthorityArn
      `Prelude.hashWithSalt` trustAnchors
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eapMethod
      `Prelude.hashWithSalt` nextPassword
      `Prelude.hashWithSalt` currentPassword
      `Prelude.hashWithSalt` securityType
      `Prelude.hashWithSalt` networkProfileArn
      `Prelude.hashWithSalt` ssid
      `Prelude.hashWithSalt` networkProfileName

instance Prelude.NFData NetworkProfile where
  rnf NetworkProfile' {..} =
    Prelude.rnf networkProfileName
      `Prelude.seq` Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf trustAnchors
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eapMethod
      `Prelude.seq` Prelude.rnf nextPassword
      `Prelude.seq` Prelude.rnf currentPassword
      `Prelude.seq` Prelude.rnf securityType
      `Prelude.seq` Prelude.rnf networkProfileArn
      `Prelude.seq` Prelude.rnf ssid
