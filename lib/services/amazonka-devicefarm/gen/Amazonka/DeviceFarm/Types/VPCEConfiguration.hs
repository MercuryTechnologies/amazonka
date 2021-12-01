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
-- Module      : Amazonka.DeviceFarm.Types.VPCEConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.VPCEConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents an Amazon Virtual Private Cloud (VPC) endpoint configuration.
--
-- /See:/ 'newVPCEConfiguration' smart constructor.
data VPCEConfiguration = VPCEConfiguration'
  { -- | The name of the VPC endpoint service running in your AWS account that
    -- you want Device Farm to test.
    vpceServiceName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the VPC endpoint configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The friendly name you give to your VPC endpoint configuration to manage
    -- your configurations more easily.
    vpceConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The DNS name that maps to the private IP address of the service you want
    -- to access.
    serviceDnsName :: Prelude.Maybe Prelude.Text,
    -- | An optional description that provides details about your VPC endpoint
    -- configuration.
    vpceConfigurationDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VPCEConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpceServiceName', 'vPCEConfiguration_vpceServiceName' - The name of the VPC endpoint service running in your AWS account that
-- you want Device Farm to test.
--
-- 'arn', 'vPCEConfiguration_arn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration.
--
-- 'vpceConfigurationName', 'vPCEConfiguration_vpceConfigurationName' - The friendly name you give to your VPC endpoint configuration to manage
-- your configurations more easily.
--
-- 'serviceDnsName', 'vPCEConfiguration_serviceDnsName' - The DNS name that maps to the private IP address of the service you want
-- to access.
--
-- 'vpceConfigurationDescription', 'vPCEConfiguration_vpceConfigurationDescription' - An optional description that provides details about your VPC endpoint
-- configuration.
newVPCEConfiguration ::
  VPCEConfiguration
newVPCEConfiguration =
  VPCEConfiguration'
    { vpceServiceName =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      vpceConfigurationName = Prelude.Nothing,
      serviceDnsName = Prelude.Nothing,
      vpceConfigurationDescription = Prelude.Nothing
    }

-- | The name of the VPC endpoint service running in your AWS account that
-- you want Device Farm to test.
vPCEConfiguration_vpceServiceName :: Lens.Lens' VPCEConfiguration (Prelude.Maybe Prelude.Text)
vPCEConfiguration_vpceServiceName = Lens.lens (\VPCEConfiguration' {vpceServiceName} -> vpceServiceName) (\s@VPCEConfiguration' {} a -> s {vpceServiceName = a} :: VPCEConfiguration)

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration.
vPCEConfiguration_arn :: Lens.Lens' VPCEConfiguration (Prelude.Maybe Prelude.Text)
vPCEConfiguration_arn = Lens.lens (\VPCEConfiguration' {arn} -> arn) (\s@VPCEConfiguration' {} a -> s {arn = a} :: VPCEConfiguration)

-- | The friendly name you give to your VPC endpoint configuration to manage
-- your configurations more easily.
vPCEConfiguration_vpceConfigurationName :: Lens.Lens' VPCEConfiguration (Prelude.Maybe Prelude.Text)
vPCEConfiguration_vpceConfigurationName = Lens.lens (\VPCEConfiguration' {vpceConfigurationName} -> vpceConfigurationName) (\s@VPCEConfiguration' {} a -> s {vpceConfigurationName = a} :: VPCEConfiguration)

-- | The DNS name that maps to the private IP address of the service you want
-- to access.
vPCEConfiguration_serviceDnsName :: Lens.Lens' VPCEConfiguration (Prelude.Maybe Prelude.Text)
vPCEConfiguration_serviceDnsName = Lens.lens (\VPCEConfiguration' {serviceDnsName} -> serviceDnsName) (\s@VPCEConfiguration' {} a -> s {serviceDnsName = a} :: VPCEConfiguration)

-- | An optional description that provides details about your VPC endpoint
-- configuration.
vPCEConfiguration_vpceConfigurationDescription :: Lens.Lens' VPCEConfiguration (Prelude.Maybe Prelude.Text)
vPCEConfiguration_vpceConfigurationDescription = Lens.lens (\VPCEConfiguration' {vpceConfigurationDescription} -> vpceConfigurationDescription) (\s@VPCEConfiguration' {} a -> s {vpceConfigurationDescription = a} :: VPCEConfiguration)

instance Core.FromJSON VPCEConfiguration where
  parseJSON =
    Core.withObject
      "VPCEConfiguration"
      ( \x ->
          VPCEConfiguration'
            Prelude.<$> (x Core..:? "vpceServiceName")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "vpceConfigurationName")
            Prelude.<*> (x Core..:? "serviceDnsName")
            Prelude.<*> (x Core..:? "vpceConfigurationDescription")
      )

instance Prelude.Hashable VPCEConfiguration where
  hashWithSalt salt' VPCEConfiguration' {..} =
    salt'
      `Prelude.hashWithSalt` vpceConfigurationDescription
      `Prelude.hashWithSalt` serviceDnsName
      `Prelude.hashWithSalt` vpceConfigurationName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` vpceServiceName

instance Prelude.NFData VPCEConfiguration where
  rnf VPCEConfiguration' {..} =
    Prelude.rnf vpceServiceName
      `Prelude.seq` Prelude.rnf vpceConfigurationDescription
      `Prelude.seq` Prelude.rnf serviceDnsName
      `Prelude.seq` Prelude.rnf vpceConfigurationName
      `Prelude.seq` Prelude.rnf arn
