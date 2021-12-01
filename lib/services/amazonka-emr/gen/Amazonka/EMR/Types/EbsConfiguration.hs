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
-- Module      : Amazonka.EMR.Types.EbsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.EbsConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.EMR.Types.EbsBlockDeviceConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon EBS configuration of a cluster instance.
--
-- /See:/ 'newEbsConfiguration' smart constructor.
data EbsConfiguration = EbsConfiguration'
  { -- | Indicates whether an Amazon EBS volume is EBS-optimized.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | An array of Amazon EBS volume specifications attached to a cluster
    -- instance.
    ebsBlockDeviceConfigs :: Prelude.Maybe [EbsBlockDeviceConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EbsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsOptimized', 'ebsConfiguration_ebsOptimized' - Indicates whether an Amazon EBS volume is EBS-optimized.
--
-- 'ebsBlockDeviceConfigs', 'ebsConfiguration_ebsBlockDeviceConfigs' - An array of Amazon EBS volume specifications attached to a cluster
-- instance.
newEbsConfiguration ::
  EbsConfiguration
newEbsConfiguration =
  EbsConfiguration'
    { ebsOptimized = Prelude.Nothing,
      ebsBlockDeviceConfigs = Prelude.Nothing
    }

-- | Indicates whether an Amazon EBS volume is EBS-optimized.
ebsConfiguration_ebsOptimized :: Lens.Lens' EbsConfiguration (Prelude.Maybe Prelude.Bool)
ebsConfiguration_ebsOptimized = Lens.lens (\EbsConfiguration' {ebsOptimized} -> ebsOptimized) (\s@EbsConfiguration' {} a -> s {ebsOptimized = a} :: EbsConfiguration)

-- | An array of Amazon EBS volume specifications attached to a cluster
-- instance.
ebsConfiguration_ebsBlockDeviceConfigs :: Lens.Lens' EbsConfiguration (Prelude.Maybe [EbsBlockDeviceConfig])
ebsConfiguration_ebsBlockDeviceConfigs = Lens.lens (\EbsConfiguration' {ebsBlockDeviceConfigs} -> ebsBlockDeviceConfigs) (\s@EbsConfiguration' {} a -> s {ebsBlockDeviceConfigs = a} :: EbsConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable EbsConfiguration where
  hashWithSalt salt' EbsConfiguration' {..} =
    salt' `Prelude.hashWithSalt` ebsBlockDeviceConfigs
      `Prelude.hashWithSalt` ebsOptimized

instance Prelude.NFData EbsConfiguration where
  rnf EbsConfiguration' {..} =
    Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf ebsBlockDeviceConfigs

instance Core.ToJSON EbsConfiguration where
  toJSON EbsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EbsOptimized" Core..=) Prelude.<$> ebsOptimized,
            ("EbsBlockDeviceConfigs" Core..=)
              Prelude.<$> ebsBlockDeviceConfigs
          ]
      )
