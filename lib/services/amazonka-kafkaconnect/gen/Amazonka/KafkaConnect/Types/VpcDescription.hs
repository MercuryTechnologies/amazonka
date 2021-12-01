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
-- Module      : Amazonka.KafkaConnect.Types.VpcDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.VpcDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The description of the VPC in which the connector resides.
--
-- /See:/ 'newVpcDescription' smart constructor.
data VpcDescription = VpcDescription'
  { -- | The security groups for the connector.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The subnets for the connector.
    subnets :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroups', 'vpcDescription_securityGroups' - The security groups for the connector.
--
-- 'subnets', 'vpcDescription_subnets' - The subnets for the connector.
newVpcDescription ::
  VpcDescription
newVpcDescription =
  VpcDescription'
    { securityGroups = Prelude.Nothing,
      subnets = Prelude.Nothing
    }

-- | The security groups for the connector.
vpcDescription_securityGroups :: Lens.Lens' VpcDescription (Prelude.Maybe [Prelude.Text])
vpcDescription_securityGroups = Lens.lens (\VpcDescription' {securityGroups} -> securityGroups) (\s@VpcDescription' {} a -> s {securityGroups = a} :: VpcDescription) Prelude.. Lens.mapping Lens.coerced

-- | The subnets for the connector.
vpcDescription_subnets :: Lens.Lens' VpcDescription (Prelude.Maybe [Prelude.Text])
vpcDescription_subnets = Lens.lens (\VpcDescription' {subnets} -> subnets) (\s@VpcDescription' {} a -> s {subnets = a} :: VpcDescription) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON VpcDescription where
  parseJSON =
    Core.withObject
      "VpcDescription"
      ( \x ->
          VpcDescription'
            Prelude.<$> (x Core..:? "securityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "subnets" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable VpcDescription where
  hashWithSalt salt' VpcDescription' {..} =
    salt' `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` securityGroups

instance Prelude.NFData VpcDescription where
  rnf VpcDescription' {..} =
    Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf subnets
