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
-- Module      : Amazonka.GroundStation.Types.SecurityDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.SecurityDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about endpoints.
--
-- /See:/ 'newSecurityDetails' smart constructor.
data SecurityDetails = SecurityDetails'
  { -- | ARN to a role needed for connecting streams to your instances.
    roleArn :: Prelude.Text,
    -- | The security groups to attach to the elastic network interfaces.
    securityGroupIds :: [Prelude.Text],
    -- | A list of subnets where AWS Ground Station places elastic network
    -- interfaces to send streams to your instances.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'securityDetails_roleArn' - ARN to a role needed for connecting streams to your instances.
--
-- 'securityGroupIds', 'securityDetails_securityGroupIds' - The security groups to attach to the elastic network interfaces.
--
-- 'subnetIds', 'securityDetails_subnetIds' - A list of subnets where AWS Ground Station places elastic network
-- interfaces to send streams to your instances.
newSecurityDetails ::
  -- | 'roleArn'
  Prelude.Text ->
  SecurityDetails
newSecurityDetails pRoleArn_ =
  SecurityDetails'
    { roleArn = pRoleArn_,
      securityGroupIds = Prelude.mempty,
      subnetIds = Prelude.mempty
    }

-- | ARN to a role needed for connecting streams to your instances.
securityDetails_roleArn :: Lens.Lens' SecurityDetails Prelude.Text
securityDetails_roleArn = Lens.lens (\SecurityDetails' {roleArn} -> roleArn) (\s@SecurityDetails' {} a -> s {roleArn = a} :: SecurityDetails)

-- | The security groups to attach to the elastic network interfaces.
securityDetails_securityGroupIds :: Lens.Lens' SecurityDetails [Prelude.Text]
securityDetails_securityGroupIds = Lens.lens (\SecurityDetails' {securityGroupIds} -> securityGroupIds) (\s@SecurityDetails' {} a -> s {securityGroupIds = a} :: SecurityDetails) Prelude.. Lens.coerced

-- | A list of subnets where AWS Ground Station places elastic network
-- interfaces to send streams to your instances.
securityDetails_subnetIds :: Lens.Lens' SecurityDetails [Prelude.Text]
securityDetails_subnetIds = Lens.lens (\SecurityDetails' {subnetIds} -> subnetIds) (\s@SecurityDetails' {} a -> s {subnetIds = a} :: SecurityDetails) Prelude.. Lens.coerced

instance Core.FromJSON SecurityDetails where
  parseJSON =
    Core.withObject
      "SecurityDetails"
      ( \x ->
          SecurityDetails'
            Prelude.<$> (x Core..: "roleArn")
            Prelude.<*> ( x Core..:? "securityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "subnetIds" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SecurityDetails where
  hashWithSalt salt' SecurityDetails' {..} =
    salt' `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData SecurityDetails where
  rnf SecurityDetails' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf securityGroupIds

instance Core.ToJSON SecurityDetails where
  toJSON SecurityDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleArn" Core..= roleArn),
            Prelude.Just
              ("securityGroupIds" Core..= securityGroupIds),
            Prelude.Just ("subnetIds" Core..= subnetIds)
          ]
      )
