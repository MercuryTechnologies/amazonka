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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceAttachment
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceIpV6AddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfacePrivateIpAddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceSecurityGroup

-- | Details about the network interface
--
-- /See:/ 'newAwsEc2NetworkInterfaceDetails' smart constructor.
data AwsEc2NetworkInterfaceDetails = AwsEc2NetworkInterfaceDetails'
  { -- | The private IPv4 addresses associated with the network interface.
    privateIpAddresses :: Prelude.Maybe [AwsEc2NetworkInterfacePrivateIpAddressDetail],
    -- | The public DNS name of the network interface.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | Security groups for the network interface.
    securityGroups :: Prelude.Maybe [AwsEc2NetworkInterfaceSecurityGroup],
    -- | Indicates whether traffic to or from the instance is validated.
    sourceDestCheck :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The network interface attachment.
    attachment :: Prelude.Maybe AwsEc2NetworkInterfaceAttachment,
    -- | The address of the Elastic IP address bound to the network interface.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 addresses associated with the network interface.
    ipV6Addresses :: Prelude.Maybe [AwsEc2NetworkInterfaceIpV6AddressDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2NetworkInterfaceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateIpAddresses', 'awsEc2NetworkInterfaceDetails_privateIpAddresses' - The private IPv4 addresses associated with the network interface.
--
-- 'publicDnsName', 'awsEc2NetworkInterfaceDetails_publicDnsName' - The public DNS name of the network interface.
--
-- 'securityGroups', 'awsEc2NetworkInterfaceDetails_securityGroups' - Security groups for the network interface.
--
-- 'sourceDestCheck', 'awsEc2NetworkInterfaceDetails_sourceDestCheck' - Indicates whether traffic to or from the instance is validated.
--
-- 'networkInterfaceId', 'awsEc2NetworkInterfaceDetails_networkInterfaceId' - The ID of the network interface.
--
-- 'attachment', 'awsEc2NetworkInterfaceDetails_attachment' - The network interface attachment.
--
-- 'publicIp', 'awsEc2NetworkInterfaceDetails_publicIp' - The address of the Elastic IP address bound to the network interface.
--
-- 'ipV6Addresses', 'awsEc2NetworkInterfaceDetails_ipV6Addresses' - The IPv6 addresses associated with the network interface.
newAwsEc2NetworkInterfaceDetails ::
  AwsEc2NetworkInterfaceDetails
newAwsEc2NetworkInterfaceDetails =
  AwsEc2NetworkInterfaceDetails'
    { privateIpAddresses =
        Prelude.Nothing,
      publicDnsName = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      sourceDestCheck = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      attachment = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      ipV6Addresses = Prelude.Nothing
    }

-- | The private IPv4 addresses associated with the network interface.
awsEc2NetworkInterfaceDetails_privateIpAddresses :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe [AwsEc2NetworkInterfacePrivateIpAddressDetail])
awsEc2NetworkInterfaceDetails_privateIpAddresses = Lens.lens (\AwsEc2NetworkInterfaceDetails' {privateIpAddresses} -> privateIpAddresses) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {privateIpAddresses = a} :: AwsEc2NetworkInterfaceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The public DNS name of the network interface.
awsEc2NetworkInterfaceDetails_publicDnsName :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceDetails_publicDnsName = Lens.lens (\AwsEc2NetworkInterfaceDetails' {publicDnsName} -> publicDnsName) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {publicDnsName = a} :: AwsEc2NetworkInterfaceDetails)

-- | Security groups for the network interface.
awsEc2NetworkInterfaceDetails_securityGroups :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe [AwsEc2NetworkInterfaceSecurityGroup])
awsEc2NetworkInterfaceDetails_securityGroups = Lens.lens (\AwsEc2NetworkInterfaceDetails' {securityGroups} -> securityGroups) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {securityGroups = a} :: AwsEc2NetworkInterfaceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether traffic to or from the instance is validated.
awsEc2NetworkInterfaceDetails_sourceDestCheck :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe Prelude.Bool)
awsEc2NetworkInterfaceDetails_sourceDestCheck = Lens.lens (\AwsEc2NetworkInterfaceDetails' {sourceDestCheck} -> sourceDestCheck) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {sourceDestCheck = a} :: AwsEc2NetworkInterfaceDetails)

-- | The ID of the network interface.
awsEc2NetworkInterfaceDetails_networkInterfaceId :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceDetails_networkInterfaceId = Lens.lens (\AwsEc2NetworkInterfaceDetails' {networkInterfaceId} -> networkInterfaceId) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {networkInterfaceId = a} :: AwsEc2NetworkInterfaceDetails)

-- | The network interface attachment.
awsEc2NetworkInterfaceDetails_attachment :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe AwsEc2NetworkInterfaceAttachment)
awsEc2NetworkInterfaceDetails_attachment = Lens.lens (\AwsEc2NetworkInterfaceDetails' {attachment} -> attachment) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {attachment = a} :: AwsEc2NetworkInterfaceDetails)

-- | The address of the Elastic IP address bound to the network interface.
awsEc2NetworkInterfaceDetails_publicIp :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceDetails_publicIp = Lens.lens (\AwsEc2NetworkInterfaceDetails' {publicIp} -> publicIp) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {publicIp = a} :: AwsEc2NetworkInterfaceDetails)

-- | The IPv6 addresses associated with the network interface.
awsEc2NetworkInterfaceDetails_ipV6Addresses :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe [AwsEc2NetworkInterfaceIpV6AddressDetail])
awsEc2NetworkInterfaceDetails_ipV6Addresses = Lens.lens (\AwsEc2NetworkInterfaceDetails' {ipV6Addresses} -> ipV6Addresses) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {ipV6Addresses = a} :: AwsEc2NetworkInterfaceDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsEc2NetworkInterfaceDetails where
  parseJSON =
    Core.withObject
      "AwsEc2NetworkInterfaceDetails"
      ( \x ->
          AwsEc2NetworkInterfaceDetails'
            Prelude.<$> ( x Core..:? "PrivateIpAddresses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PublicDnsName")
            Prelude.<*> (x Core..:? "SecurityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SourceDestCheck")
            Prelude.<*> (x Core..:? "NetworkInterfaceId")
            Prelude.<*> (x Core..:? "Attachment")
            Prelude.<*> (x Core..:? "PublicIp")
            Prelude.<*> (x Core..:? "IpV6Addresses" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEc2NetworkInterfaceDetails
  where
  hashWithSalt salt' AwsEc2NetworkInterfaceDetails' {..} =
    salt' `Prelude.hashWithSalt` ipV6Addresses
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` attachment
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` sourceDestCheck
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` publicDnsName
      `Prelude.hashWithSalt` privateIpAddresses

instance Prelude.NFData AwsEc2NetworkInterfaceDetails where
  rnf AwsEc2NetworkInterfaceDetails' {..} =
    Prelude.rnf privateIpAddresses
      `Prelude.seq` Prelude.rnf ipV6Addresses
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf sourceDestCheck
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf publicDnsName

instance Core.ToJSON AwsEc2NetworkInterfaceDetails where
  toJSON AwsEc2NetworkInterfaceDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PrivateIpAddresses" Core..=)
              Prelude.<$> privateIpAddresses,
            ("PublicDnsName" Core..=) Prelude.<$> publicDnsName,
            ("SecurityGroups" Core..=)
              Prelude.<$> securityGroups,
            ("SourceDestCheck" Core..=)
              Prelude.<$> sourceDestCheck,
            ("NetworkInterfaceId" Core..=)
              Prelude.<$> networkInterfaceId,
            ("Attachment" Core..=) Prelude.<$> attachment,
            ("PublicIp" Core..=) Prelude.<$> publicIp,
            ("IpV6Addresses" Core..=) Prelude.<$> ipV6Addresses
          ]
      )
