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
-- Module      : Amazonka.MwAA.Types.NetworkConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.NetworkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The VPC networking components used to secure and enable network traffic
-- between the AWS resources for your environment. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/networking-about.html About networking on Amazon MWAA>.
--
-- /See:/ 'newNetworkConfiguration' smart constructor.
data NetworkConfiguration = NetworkConfiguration'
  { -- | A list of 1 or more security group IDs. Accepts up to 5 security group
    -- IDs. A security group must be attached to the same VPC as the subnets.
    -- To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/vpc-security.html Security in your VPC on Amazon MWAA>.
    securityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of 2 subnet IDs. __Required__ to create an environment. Must be
    -- private subnets in two different availability zones. A subnet must be
    -- attached to the same VPC as the security group.
    subnetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'networkConfiguration_securityGroupIds' - A list of 1 or more security group IDs. Accepts up to 5 security group
-- IDs. A security group must be attached to the same VPC as the subnets.
-- To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/vpc-security.html Security in your VPC on Amazon MWAA>.
--
-- 'subnetIds', 'networkConfiguration_subnetIds' - A list of 2 subnet IDs. __Required__ to create an environment. Must be
-- private subnets in two different availability zones. A subnet must be
-- attached to the same VPC as the security group.
newNetworkConfiguration ::
  NetworkConfiguration
newNetworkConfiguration =
  NetworkConfiguration'
    { securityGroupIds =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | A list of 1 or more security group IDs. Accepts up to 5 security group
-- IDs. A security group must be attached to the same VPC as the subnets.
-- To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/vpc-security.html Security in your VPC on Amazon MWAA>.
networkConfiguration_securityGroupIds :: Lens.Lens' NetworkConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
networkConfiguration_securityGroupIds = Lens.lens (\NetworkConfiguration' {securityGroupIds} -> securityGroupIds) (\s@NetworkConfiguration' {} a -> s {securityGroupIds = a} :: NetworkConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of 2 subnet IDs. __Required__ to create an environment. Must be
-- private subnets in two different availability zones. A subnet must be
-- attached to the same VPC as the security group.
networkConfiguration_subnetIds :: Lens.Lens' NetworkConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
networkConfiguration_subnetIds = Lens.lens (\NetworkConfiguration' {subnetIds} -> subnetIds) (\s@NetworkConfiguration' {} a -> s {subnetIds = a} :: NetworkConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON NetworkConfiguration where
  parseJSON =
    Core.withObject
      "NetworkConfiguration"
      ( \x ->
          NetworkConfiguration'
            Prelude.<$> (x Core..:? "SecurityGroupIds")
            Prelude.<*> (x Core..:? "SubnetIds")
      )

instance Prelude.Hashable NetworkConfiguration where
  hashWithSalt salt' NetworkConfiguration' {..} =
    salt' `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` securityGroupIds

instance Prelude.NFData NetworkConfiguration where
  rnf NetworkConfiguration' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance Core.ToJSON NetworkConfiguration where
  toJSON NetworkConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("SubnetIds" Core..=) Prelude.<$> subnetIds
          ]
      )
