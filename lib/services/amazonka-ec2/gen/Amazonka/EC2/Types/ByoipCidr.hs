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
-- Module      : Amazonka.EC2.Types.ByoipCidr
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ByoipCidr where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ByoipCidrState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an address range that is provisioned for use with your
-- Amazon Web Services resources through bring your own IP addresses
-- (BYOIP).
--
-- /See:/ 'newByoipCidr' smart constructor.
data ByoipCidr = ByoipCidr'
  { -- | The state of the address pool.
    state :: Prelude.Maybe ByoipCidrState,
    -- | The address range, in CIDR notation.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | Upon success, contains the ID of the address pool. Otherwise, contains
    -- an error message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The description of the address range.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ByoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'byoipCidr_state' - The state of the address pool.
--
-- 'cidr', 'byoipCidr_cidr' - The address range, in CIDR notation.
--
-- 'statusMessage', 'byoipCidr_statusMessage' - Upon success, contains the ID of the address pool. Otherwise, contains
-- an error message.
--
-- 'description', 'byoipCidr_description' - The description of the address range.
newByoipCidr ::
  ByoipCidr
newByoipCidr =
  ByoipCidr'
    { state = Prelude.Nothing,
      cidr = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The state of the address pool.
byoipCidr_state :: Lens.Lens' ByoipCidr (Prelude.Maybe ByoipCidrState)
byoipCidr_state = Lens.lens (\ByoipCidr' {state} -> state) (\s@ByoipCidr' {} a -> s {state = a} :: ByoipCidr)

-- | The address range, in CIDR notation.
byoipCidr_cidr :: Lens.Lens' ByoipCidr (Prelude.Maybe Prelude.Text)
byoipCidr_cidr = Lens.lens (\ByoipCidr' {cidr} -> cidr) (\s@ByoipCidr' {} a -> s {cidr = a} :: ByoipCidr)

-- | Upon success, contains the ID of the address pool. Otherwise, contains
-- an error message.
byoipCidr_statusMessage :: Lens.Lens' ByoipCidr (Prelude.Maybe Prelude.Text)
byoipCidr_statusMessage = Lens.lens (\ByoipCidr' {statusMessage} -> statusMessage) (\s@ByoipCidr' {} a -> s {statusMessage = a} :: ByoipCidr)

-- | The description of the address range.
byoipCidr_description :: Lens.Lens' ByoipCidr (Prelude.Maybe Prelude.Text)
byoipCidr_description = Lens.lens (\ByoipCidr' {description} -> description) (\s@ByoipCidr' {} a -> s {description = a} :: ByoipCidr)

instance Core.FromXML ByoipCidr where
  parseXML x =
    ByoipCidr'
      Prelude.<$> (x Core..@? "state")
      Prelude.<*> (x Core..@? "cidr")
      Prelude.<*> (x Core..@? "statusMessage")
      Prelude.<*> (x Core..@? "description")

instance Prelude.Hashable ByoipCidr where
  hashWithSalt salt' ByoipCidr' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` state

instance Prelude.NFData ByoipCidr where
  rnf ByoipCidr' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf cidr
