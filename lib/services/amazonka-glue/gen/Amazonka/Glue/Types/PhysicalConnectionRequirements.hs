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
-- Module      : Amazonka.Glue.Types.PhysicalConnectionRequirements
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PhysicalConnectionRequirements where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the physical requirements for a connection.
--
-- /See:/ 'newPhysicalConnectionRequirements' smart constructor.
data PhysicalConnectionRequirements = PhysicalConnectionRequirements'
  { -- | The security group ID list used by the connection.
    securityGroupIdList :: Prelude.Maybe [Prelude.Text],
    -- | The subnet ID used by the connection.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The connection\'s Availability Zone. This field is redundant because the
    -- specified subnet implies the Availability Zone to be used. Currently the
    -- field must be populated, but it will be deprecated in the future.
    availabilityZone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhysicalConnectionRequirements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIdList', 'physicalConnectionRequirements_securityGroupIdList' - The security group ID list used by the connection.
--
-- 'subnetId', 'physicalConnectionRequirements_subnetId' - The subnet ID used by the connection.
--
-- 'availabilityZone', 'physicalConnectionRequirements_availabilityZone' - The connection\'s Availability Zone. This field is redundant because the
-- specified subnet implies the Availability Zone to be used. Currently the
-- field must be populated, but it will be deprecated in the future.
newPhysicalConnectionRequirements ::
  PhysicalConnectionRequirements
newPhysicalConnectionRequirements =
  PhysicalConnectionRequirements'
    { securityGroupIdList =
        Prelude.Nothing,
      subnetId = Prelude.Nothing,
      availabilityZone = Prelude.Nothing
    }

-- | The security group ID list used by the connection.
physicalConnectionRequirements_securityGroupIdList :: Lens.Lens' PhysicalConnectionRequirements (Prelude.Maybe [Prelude.Text])
physicalConnectionRequirements_securityGroupIdList = Lens.lens (\PhysicalConnectionRequirements' {securityGroupIdList} -> securityGroupIdList) (\s@PhysicalConnectionRequirements' {} a -> s {securityGroupIdList = a} :: PhysicalConnectionRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The subnet ID used by the connection.
physicalConnectionRequirements_subnetId :: Lens.Lens' PhysicalConnectionRequirements (Prelude.Maybe Prelude.Text)
physicalConnectionRequirements_subnetId = Lens.lens (\PhysicalConnectionRequirements' {subnetId} -> subnetId) (\s@PhysicalConnectionRequirements' {} a -> s {subnetId = a} :: PhysicalConnectionRequirements)

-- | The connection\'s Availability Zone. This field is redundant because the
-- specified subnet implies the Availability Zone to be used. Currently the
-- field must be populated, but it will be deprecated in the future.
physicalConnectionRequirements_availabilityZone :: Lens.Lens' PhysicalConnectionRequirements (Prelude.Maybe Prelude.Text)
physicalConnectionRequirements_availabilityZone = Lens.lens (\PhysicalConnectionRequirements' {availabilityZone} -> availabilityZone) (\s@PhysicalConnectionRequirements' {} a -> s {availabilityZone = a} :: PhysicalConnectionRequirements)

instance Core.FromJSON PhysicalConnectionRequirements where
  parseJSON =
    Core.withObject
      "PhysicalConnectionRequirements"
      ( \x ->
          PhysicalConnectionRequirements'
            Prelude.<$> ( x Core..:? "SecurityGroupIdList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SubnetId")
            Prelude.<*> (x Core..:? "AvailabilityZone")
      )

instance
  Prelude.Hashable
    PhysicalConnectionRequirements
  where
  hashWithSalt
    salt'
    PhysicalConnectionRequirements' {..} =
      salt' `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` subnetId
        `Prelude.hashWithSalt` securityGroupIdList

instance
  Prelude.NFData
    PhysicalConnectionRequirements
  where
  rnf PhysicalConnectionRequirements' {..} =
    Prelude.rnf securityGroupIdList
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf subnetId

instance Core.ToJSON PhysicalConnectionRequirements where
  toJSON PhysicalConnectionRequirements' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIdList" Core..=)
              Prelude.<$> securityGroupIdList,
            ("SubnetId" Core..=) Prelude.<$> subnetId,
            ("AvailabilityZone" Core..=)
              Prelude.<$> availabilityZone
          ]
      )
