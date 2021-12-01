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
-- Module      : Amazonka.CostExplorer.Types.ReservationUtilizationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ReservationUtilizationGroup where

import qualified Amazonka.Core as Core
import Amazonka.CostExplorer.Types.ReservationAggregates
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A group of reservations that share a set of attributes.
--
-- /See:/ 'newReservationUtilizationGroup' smart constructor.
data ReservationUtilizationGroup = ReservationUtilizationGroup'
  { -- | The value of a specific reservation attribute.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key for a specific reservation attribute.
    key :: Prelude.Maybe Prelude.Text,
    -- | The attributes for this group of reservations.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | How much you used this group of reservations.
    utilization :: Prelude.Maybe ReservationAggregates
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservationUtilizationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'reservationUtilizationGroup_value' - The value of a specific reservation attribute.
--
-- 'key', 'reservationUtilizationGroup_key' - The key for a specific reservation attribute.
--
-- 'attributes', 'reservationUtilizationGroup_attributes' - The attributes for this group of reservations.
--
-- 'utilization', 'reservationUtilizationGroup_utilization' - How much you used this group of reservations.
newReservationUtilizationGroup ::
  ReservationUtilizationGroup
newReservationUtilizationGroup =
  ReservationUtilizationGroup'
    { value =
        Prelude.Nothing,
      key = Prelude.Nothing,
      attributes = Prelude.Nothing,
      utilization = Prelude.Nothing
    }

-- | The value of a specific reservation attribute.
reservationUtilizationGroup_value :: Lens.Lens' ReservationUtilizationGroup (Prelude.Maybe Prelude.Text)
reservationUtilizationGroup_value = Lens.lens (\ReservationUtilizationGroup' {value} -> value) (\s@ReservationUtilizationGroup' {} a -> s {value = a} :: ReservationUtilizationGroup)

-- | The key for a specific reservation attribute.
reservationUtilizationGroup_key :: Lens.Lens' ReservationUtilizationGroup (Prelude.Maybe Prelude.Text)
reservationUtilizationGroup_key = Lens.lens (\ReservationUtilizationGroup' {key} -> key) (\s@ReservationUtilizationGroup' {} a -> s {key = a} :: ReservationUtilizationGroup)

-- | The attributes for this group of reservations.
reservationUtilizationGroup_attributes :: Lens.Lens' ReservationUtilizationGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
reservationUtilizationGroup_attributes = Lens.lens (\ReservationUtilizationGroup' {attributes} -> attributes) (\s@ReservationUtilizationGroup' {} a -> s {attributes = a} :: ReservationUtilizationGroup) Prelude.. Lens.mapping Lens.coerced

-- | How much you used this group of reservations.
reservationUtilizationGroup_utilization :: Lens.Lens' ReservationUtilizationGroup (Prelude.Maybe ReservationAggregates)
reservationUtilizationGroup_utilization = Lens.lens (\ReservationUtilizationGroup' {utilization} -> utilization) (\s@ReservationUtilizationGroup' {} a -> s {utilization = a} :: ReservationUtilizationGroup)

instance Core.FromJSON ReservationUtilizationGroup where
  parseJSON =
    Core.withObject
      "ReservationUtilizationGroup"
      ( \x ->
          ReservationUtilizationGroup'
            Prelude.<$> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Utilization")
      )

instance Prelude.Hashable ReservationUtilizationGroup where
  hashWithSalt salt' ReservationUtilizationGroup' {..} =
    salt' `Prelude.hashWithSalt` utilization
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData ReservationUtilizationGroup where
  rnf ReservationUtilizationGroup' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf utilization
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf key
