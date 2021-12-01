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
-- Module      : Amazonka.EC2.Types.AnalysisSecurityGroupRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AnalysisSecurityGroupRule where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PortRange
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a security group rule.
--
-- /See:/ 'newAnalysisSecurityGroupRule' smart constructor.
data AnalysisSecurityGroupRule = AnalysisSecurityGroupRule'
  { -- | The direction. The following are possible values:
    --
    -- -   egress
    --
    -- -   ingress
    direction :: Prelude.Maybe Prelude.Text,
    -- | The protocol name.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The port range.
    portRange :: Prelude.Maybe PortRange,
    -- | The security group ID.
    securityGroupId :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 address range, in CIDR notation.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | The prefix list ID.
    prefixListId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisSecurityGroupRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'direction', 'analysisSecurityGroupRule_direction' - The direction. The following are possible values:
--
-- -   egress
--
-- -   ingress
--
-- 'protocol', 'analysisSecurityGroupRule_protocol' - The protocol name.
--
-- 'portRange', 'analysisSecurityGroupRule_portRange' - The port range.
--
-- 'securityGroupId', 'analysisSecurityGroupRule_securityGroupId' - The security group ID.
--
-- 'cidr', 'analysisSecurityGroupRule_cidr' - The IPv4 address range, in CIDR notation.
--
-- 'prefixListId', 'analysisSecurityGroupRule_prefixListId' - The prefix list ID.
newAnalysisSecurityGroupRule ::
  AnalysisSecurityGroupRule
newAnalysisSecurityGroupRule =
  AnalysisSecurityGroupRule'
    { direction =
        Prelude.Nothing,
      protocol = Prelude.Nothing,
      portRange = Prelude.Nothing,
      securityGroupId = Prelude.Nothing,
      cidr = Prelude.Nothing,
      prefixListId = Prelude.Nothing
    }

-- | The direction. The following are possible values:
--
-- -   egress
--
-- -   ingress
analysisSecurityGroupRule_direction :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe Prelude.Text)
analysisSecurityGroupRule_direction = Lens.lens (\AnalysisSecurityGroupRule' {direction} -> direction) (\s@AnalysisSecurityGroupRule' {} a -> s {direction = a} :: AnalysisSecurityGroupRule)

-- | The protocol name.
analysisSecurityGroupRule_protocol :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe Prelude.Text)
analysisSecurityGroupRule_protocol = Lens.lens (\AnalysisSecurityGroupRule' {protocol} -> protocol) (\s@AnalysisSecurityGroupRule' {} a -> s {protocol = a} :: AnalysisSecurityGroupRule)

-- | The port range.
analysisSecurityGroupRule_portRange :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe PortRange)
analysisSecurityGroupRule_portRange = Lens.lens (\AnalysisSecurityGroupRule' {portRange} -> portRange) (\s@AnalysisSecurityGroupRule' {} a -> s {portRange = a} :: AnalysisSecurityGroupRule)

-- | The security group ID.
analysisSecurityGroupRule_securityGroupId :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe Prelude.Text)
analysisSecurityGroupRule_securityGroupId = Lens.lens (\AnalysisSecurityGroupRule' {securityGroupId} -> securityGroupId) (\s@AnalysisSecurityGroupRule' {} a -> s {securityGroupId = a} :: AnalysisSecurityGroupRule)

-- | The IPv4 address range, in CIDR notation.
analysisSecurityGroupRule_cidr :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe Prelude.Text)
analysisSecurityGroupRule_cidr = Lens.lens (\AnalysisSecurityGroupRule' {cidr} -> cidr) (\s@AnalysisSecurityGroupRule' {} a -> s {cidr = a} :: AnalysisSecurityGroupRule)

-- | The prefix list ID.
analysisSecurityGroupRule_prefixListId :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe Prelude.Text)
analysisSecurityGroupRule_prefixListId = Lens.lens (\AnalysisSecurityGroupRule' {prefixListId} -> prefixListId) (\s@AnalysisSecurityGroupRule' {} a -> s {prefixListId = a} :: AnalysisSecurityGroupRule)

instance Core.FromXML AnalysisSecurityGroupRule where
  parseXML x =
    AnalysisSecurityGroupRule'
      Prelude.<$> (x Core..@? "direction")
      Prelude.<*> (x Core..@? "protocol")
      Prelude.<*> (x Core..@? "portRange")
      Prelude.<*> (x Core..@? "securityGroupId")
      Prelude.<*> (x Core..@? "cidr")
      Prelude.<*> (x Core..@? "prefixListId")

instance Prelude.Hashable AnalysisSecurityGroupRule where
  hashWithSalt salt' AnalysisSecurityGroupRule' {..} =
    salt' `Prelude.hashWithSalt` prefixListId
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` securityGroupId
      `Prelude.hashWithSalt` portRange
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` direction

instance Prelude.NFData AnalysisSecurityGroupRule where
  rnf AnalysisSecurityGroupRule' {..} =
    Prelude.rnf direction
      `Prelude.seq` Prelude.rnf prefixListId
      `Prelude.seq` Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf securityGroupId
      `Prelude.seq` Prelude.rnf portRange
      `Prelude.seq` Prelude.rnf protocol
