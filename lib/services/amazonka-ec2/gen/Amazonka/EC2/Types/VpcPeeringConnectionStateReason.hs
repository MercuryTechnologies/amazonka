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
-- Module      : Amazonka.EC2.Types.VpcPeeringConnectionStateReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpcPeeringConnectionStateReason where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VpcPeeringConnectionStateReasonCode
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of a VPC peering connection.
--
-- /See:/ 'newVpcPeeringConnectionStateReason' smart constructor.
data VpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason'
  { -- | The status of the VPC peering connection.
    code :: Prelude.Maybe VpcPeeringConnectionStateReasonCode,
    -- | A message that provides more information about the status, if
    -- applicable.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcPeeringConnectionStateReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'vpcPeeringConnectionStateReason_code' - The status of the VPC peering connection.
--
-- 'message', 'vpcPeeringConnectionStateReason_message' - A message that provides more information about the status, if
-- applicable.
newVpcPeeringConnectionStateReason ::
  VpcPeeringConnectionStateReason
newVpcPeeringConnectionStateReason =
  VpcPeeringConnectionStateReason'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The status of the VPC peering connection.
vpcPeeringConnectionStateReason_code :: Lens.Lens' VpcPeeringConnectionStateReason (Prelude.Maybe VpcPeeringConnectionStateReasonCode)
vpcPeeringConnectionStateReason_code = Lens.lens (\VpcPeeringConnectionStateReason' {code} -> code) (\s@VpcPeeringConnectionStateReason' {} a -> s {code = a} :: VpcPeeringConnectionStateReason)

-- | A message that provides more information about the status, if
-- applicable.
vpcPeeringConnectionStateReason_message :: Lens.Lens' VpcPeeringConnectionStateReason (Prelude.Maybe Prelude.Text)
vpcPeeringConnectionStateReason_message = Lens.lens (\VpcPeeringConnectionStateReason' {message} -> message) (\s@VpcPeeringConnectionStateReason' {} a -> s {message = a} :: VpcPeeringConnectionStateReason)

instance Core.FromXML VpcPeeringConnectionStateReason where
  parseXML x =
    VpcPeeringConnectionStateReason'
      Prelude.<$> (x Core..@? "code")
      Prelude.<*> (x Core..@? "message")

instance
  Prelude.Hashable
    VpcPeeringConnectionStateReason
  where
  hashWithSalt
    salt'
    VpcPeeringConnectionStateReason' {..} =
      salt' `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` code

instance
  Prelude.NFData
    VpcPeeringConnectionStateReason
  where
  rnf VpcPeeringConnectionStateReason' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
