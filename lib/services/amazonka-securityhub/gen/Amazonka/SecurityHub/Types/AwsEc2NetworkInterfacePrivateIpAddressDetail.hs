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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2NetworkInterfacePrivateIpAddressDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2NetworkInterfacePrivateIpAddressDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a private IPv4 address that is with the
-- network interface.
--
-- /See:/ 'newAwsEc2NetworkInterfacePrivateIpAddressDetail' smart constructor.
data AwsEc2NetworkInterfacePrivateIpAddressDetail = AwsEc2NetworkInterfacePrivateIpAddressDetail'
  { -- | The IP address.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The private DNS name for the IP address.
    privateDnsName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2NetworkInterfacePrivateIpAddressDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateIpAddress', 'awsEc2NetworkInterfacePrivateIpAddressDetail_privateIpAddress' - The IP address.
--
-- 'privateDnsName', 'awsEc2NetworkInterfacePrivateIpAddressDetail_privateDnsName' - The private DNS name for the IP address.
newAwsEc2NetworkInterfacePrivateIpAddressDetail ::
  AwsEc2NetworkInterfacePrivateIpAddressDetail
newAwsEc2NetworkInterfacePrivateIpAddressDetail =
  AwsEc2NetworkInterfacePrivateIpAddressDetail'
    { privateIpAddress =
        Prelude.Nothing,
      privateDnsName =
        Prelude.Nothing
    }

-- | The IP address.
awsEc2NetworkInterfacePrivateIpAddressDetail_privateIpAddress :: Lens.Lens' AwsEc2NetworkInterfacePrivateIpAddressDetail (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfacePrivateIpAddressDetail_privateIpAddress = Lens.lens (\AwsEc2NetworkInterfacePrivateIpAddressDetail' {privateIpAddress} -> privateIpAddress) (\s@AwsEc2NetworkInterfacePrivateIpAddressDetail' {} a -> s {privateIpAddress = a} :: AwsEc2NetworkInterfacePrivateIpAddressDetail)

-- | The private DNS name for the IP address.
awsEc2NetworkInterfacePrivateIpAddressDetail_privateDnsName :: Lens.Lens' AwsEc2NetworkInterfacePrivateIpAddressDetail (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfacePrivateIpAddressDetail_privateDnsName = Lens.lens (\AwsEc2NetworkInterfacePrivateIpAddressDetail' {privateDnsName} -> privateDnsName) (\s@AwsEc2NetworkInterfacePrivateIpAddressDetail' {} a -> s {privateDnsName = a} :: AwsEc2NetworkInterfacePrivateIpAddressDetail)

instance
  Core.FromJSON
    AwsEc2NetworkInterfacePrivateIpAddressDetail
  where
  parseJSON =
    Core.withObject
      "AwsEc2NetworkInterfacePrivateIpAddressDetail"
      ( \x ->
          AwsEc2NetworkInterfacePrivateIpAddressDetail'
            Prelude.<$> (x Core..:? "PrivateIpAddress")
              Prelude.<*> (x Core..:? "PrivateDnsName")
      )

instance
  Prelude.Hashable
    AwsEc2NetworkInterfacePrivateIpAddressDetail
  where
  hashWithSalt
    salt'
    AwsEc2NetworkInterfacePrivateIpAddressDetail' {..} =
      salt' `Prelude.hashWithSalt` privateDnsName
        `Prelude.hashWithSalt` privateIpAddress

instance
  Prelude.NFData
    AwsEc2NetworkInterfacePrivateIpAddressDetail
  where
  rnf AwsEc2NetworkInterfacePrivateIpAddressDetail' {..} =
    Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf privateDnsName

instance
  Core.ToJSON
    AwsEc2NetworkInterfacePrivateIpAddressDetail
  where
  toJSON
    AwsEc2NetworkInterfacePrivateIpAddressDetail' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("PrivateIpAddress" Core..=)
                Prelude.<$> privateIpAddress,
              ("PrivateDnsName" Core..=)
                Prelude.<$> privateDnsName
            ]
        )
