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
-- Module      : Amazonka.EC2.Types.SubnetCidrReservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SubnetCidrReservation where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SubnetCidrReservationType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a subnet CIDR reservation.
--
-- /See:/ 'newSubnetCidrReservation' smart constructor.
data SubnetCidrReservation = SubnetCidrReservation'
  { -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account that owns the subnet CIDR reservation.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The CIDR that has been reserved.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet CIDR reservation.
    subnetCidrReservationId :: Prelude.Maybe Prelude.Text,
    -- | The type of reservation.
    reservationType :: Prelude.Maybe SubnetCidrReservationType,
    -- | The description assigned to the subnet CIDR reservation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the subnet CIDR reservation.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubnetCidrReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetId', 'subnetCidrReservation_subnetId' - The ID of the subnet.
--
-- 'ownerId', 'subnetCidrReservation_ownerId' - The ID of the account that owns the subnet CIDR reservation.
--
-- 'cidr', 'subnetCidrReservation_cidr' - The CIDR that has been reserved.
--
-- 'subnetCidrReservationId', 'subnetCidrReservation_subnetCidrReservationId' - The ID of the subnet CIDR reservation.
--
-- 'reservationType', 'subnetCidrReservation_reservationType' - The type of reservation.
--
-- 'description', 'subnetCidrReservation_description' - The description assigned to the subnet CIDR reservation.
--
-- 'tags', 'subnetCidrReservation_tags' - The tags assigned to the subnet CIDR reservation.
newSubnetCidrReservation ::
  SubnetCidrReservation
newSubnetCidrReservation =
  SubnetCidrReservation'
    { subnetId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      cidr = Prelude.Nothing,
      subnetCidrReservationId = Prelude.Nothing,
      reservationType = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the subnet.
subnetCidrReservation_subnetId :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe Prelude.Text)
subnetCidrReservation_subnetId = Lens.lens (\SubnetCidrReservation' {subnetId} -> subnetId) (\s@SubnetCidrReservation' {} a -> s {subnetId = a} :: SubnetCidrReservation)

-- | The ID of the account that owns the subnet CIDR reservation.
subnetCidrReservation_ownerId :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe Prelude.Text)
subnetCidrReservation_ownerId = Lens.lens (\SubnetCidrReservation' {ownerId} -> ownerId) (\s@SubnetCidrReservation' {} a -> s {ownerId = a} :: SubnetCidrReservation)

-- | The CIDR that has been reserved.
subnetCidrReservation_cidr :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe Prelude.Text)
subnetCidrReservation_cidr = Lens.lens (\SubnetCidrReservation' {cidr} -> cidr) (\s@SubnetCidrReservation' {} a -> s {cidr = a} :: SubnetCidrReservation)

-- | The ID of the subnet CIDR reservation.
subnetCidrReservation_subnetCidrReservationId :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe Prelude.Text)
subnetCidrReservation_subnetCidrReservationId = Lens.lens (\SubnetCidrReservation' {subnetCidrReservationId} -> subnetCidrReservationId) (\s@SubnetCidrReservation' {} a -> s {subnetCidrReservationId = a} :: SubnetCidrReservation)

-- | The type of reservation.
subnetCidrReservation_reservationType :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe SubnetCidrReservationType)
subnetCidrReservation_reservationType = Lens.lens (\SubnetCidrReservation' {reservationType} -> reservationType) (\s@SubnetCidrReservation' {} a -> s {reservationType = a} :: SubnetCidrReservation)

-- | The description assigned to the subnet CIDR reservation.
subnetCidrReservation_description :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe Prelude.Text)
subnetCidrReservation_description = Lens.lens (\SubnetCidrReservation' {description} -> description) (\s@SubnetCidrReservation' {} a -> s {description = a} :: SubnetCidrReservation)

-- | The tags assigned to the subnet CIDR reservation.
subnetCidrReservation_tags :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe [Tag])
subnetCidrReservation_tags = Lens.lens (\SubnetCidrReservation' {tags} -> tags) (\s@SubnetCidrReservation' {} a -> s {tags = a} :: SubnetCidrReservation) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML SubnetCidrReservation where
  parseXML x =
    SubnetCidrReservation'
      Prelude.<$> (x Core..@? "subnetId")
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> (x Core..@? "cidr")
      Prelude.<*> (x Core..@? "subnetCidrReservationId")
      Prelude.<*> (x Core..@? "reservationType")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable SubnetCidrReservation where
  hashWithSalt salt' SubnetCidrReservation' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` reservationType
      `Prelude.hashWithSalt` subnetCidrReservationId
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData SubnetCidrReservation where
  rnf SubnetCidrReservation' {..} =
    Prelude.rnf subnetId `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf reservationType
      `Prelude.seq` Prelude.rnf subnetCidrReservationId
      `Prelude.seq` Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf ownerId
